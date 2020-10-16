/*
 * Original Authors: Ken Duffy, HoChan Cheon, rewritten in C by David Malone.
 * An approximate SEIR epidemic model with sub-compartments in E and I
 * Adjusted for two population groups by David Malone and given an R interface.
 */

#include <assert.h>
#include <math.h>
#include <stdlib.h>
#include <stdio.h>
#include <time.h>

#include <R.h>
#include <Rinternals.h>

#define E_length 5	/* subcompartments of E - adjustable */
#define I_length 5	/* subcompartments of I - adjustable */

struct compartments {
	int S, E, I, R;
};

struct subcompartments {
	int S, E[E_length], I[I_length], R;
};

struct simresult {
	int steps;
	double *Time;
	struct compartments *O, *Y;
};

struct simresult seir_model_erlang(double , double , double , const struct compartments *, const struct compartments *, const double mean_holding_times[2], const double beta[2][2][2], const struct subcompartments *S, const struct subcompartments *);

void freeresult(struct simresult res);

SEXP twoages(SEXP YS, SEXP YE, SEXP YI, SEXP YR, SEXP OS, SEXP OE, SEXP OI, SEXP OR, SEXP YR0Y, SEXP YR0O, SEXP OR0Y, SEXP OR0O) {
	struct compartments Y, O;
	double R0[2][2];
	static int initialised = 0;

	Y.S = asInteger(YS);
	Y.E = asInteger(YE);
	Y.I = asInteger(YI);
	Y.R = asInteger(YR);
	O.S = asInteger(OS);
	O.E = asInteger(OE);
	O.I = asInteger(OI);
	O.R = asInteger(OR);
	R0[0][0] = asReal(YR0Y);
	R0[0][1] = asReal(YR0O);
	R0[1][0] = asReal(OR0Y);
	/* R0[1][1] = asReal(OR0Y); */
	R0[1][1] = asReal(OR0O);

	if (!initialised) {
		srand48(time(NULL));
		initialised = 1;
	}

	/* by default, assign initial Exposed and Infectious people in the E_1 and I_1, respectively */
	struct subcompartments SYS_Y, SYS_O;
	SYS_Y.S = Y.S;
	SYS_Y.E[0] = Y.E/E_length + Y.E%E_length;
	for (int j = 1; j < E_length; j++)
		SYS_Y.E[j] = Y.E/E_length;
	SYS_Y.I[0] = Y.I/I_length + Y.I%I_length;
	for (int j = 1; j < I_length; j++)
		SYS_Y.I[j] = Y.I/I_length;
	SYS_Y.R = Y.R;
	SYS_O.S = O.S;
	SYS_O.E[0] = O.E/E_length + O.E%E_length;
	for (int j = 1; j < E_length; j++)
		SYS_O.E[j] = O.E/E_length;
	SYS_O.I[0] = O.I/I_length + O.I%I_length;
	for (int j = 1; j < I_length; j++)
		SYS_O.I[j] = O.I/I_length;
	SYS_O.R = O.R;

	/* Epidemic model setup: parameters */
	int N_phases;

	N_phases = 1; /* number of phases (e.g. intervention at t=t*) */

	/* equal mean holding times for E and I compartments (mean(E+I) = 14days) */
	double mean_holding_times[2];
	mean_holding_times[0] = 6.6; /* mean holding times at compartment E */
	mean_holding_times[1] = 7.4; /* mean holding times at compartment I */

	double total_holding_times, beta[2][2][2];
	total_holding_times = mean_holding_times[0] + mean_holding_times[1];
	/* assuming beta_E = beta_I */
	beta[0][0][0] = R0[0][0] / total_holding_times;
	beta[0][0][1] = R0[0][1] / total_holding_times;
	beta[0][1][0] = R0[1][0] / total_holding_times;
	beta[0][1][1] = R0[1][1] / total_holding_times;
	beta[1][0][0] = R0[0][0] / total_holding_times;
	beta[1][0][1] = R0[0][1] / total_holding_times;
	beta[1][1][0] = R0[1][0] / total_holding_times;
	beta[1][1][1] = R0[1][1] / total_holding_times;

	/* initialise simulation conditions */
	double t, dt, t_phase;
	t = 0;
	dt = 1;
	t_phase = INFINITY;

	/* Run simulation */
	struct simresult realisation;

	for(int i = 0; i < N_phases; i++) {
		if (i == 0) { /* for first phase of the epimedic */
			realisation = seir_model_erlang(t_phase, t, dt, &Y, &O, mean_holding_times, beta, &SYS_Y, &SYS_O);
		} else {
			/* TODO: pass results from the first phase with different infection parameters */
			assert(0);
		}
	}

	/* Put together results for R */
	SEXP res = PROTECT(allocVector(VECSXP, 9));

	SEXP resT = PROTECT(allocVector(REALSXP, realisation.steps));
	SEXP resYS = PROTECT(allocVector(INTSXP, realisation.steps));
	SEXP resYE = PROTECT(allocVector(INTSXP, realisation.steps));
	SEXP resYI = PROTECT(allocVector(INTSXP, realisation.steps));
	SEXP resYR = PROTECT(allocVector(INTSXP, realisation.steps));
	SEXP resOS = PROTECT(allocVector(INTSXP, realisation.steps));
	SEXP resOE = PROTECT(allocVector(INTSXP, realisation.steps));
	SEXP resOI = PROTECT(allocVector(INTSXP, realisation.steps));
	SEXP resOR = PROTECT(allocVector(INTSXP, realisation.steps));

	SET_VECTOR_ELT(res, 0, resT);
	SET_VECTOR_ELT(res, 1, resYS);
	SET_VECTOR_ELT(res, 2, resYE);
	SET_VECTOR_ELT(res, 3, resYI);
	SET_VECTOR_ELT(res, 4, resYR);
	SET_VECTOR_ELT(res, 5, resOS);
	SET_VECTOR_ELT(res, 6, resOE);
	SET_VECTOR_ELT(res, 7, resOI);
	SET_VECTOR_ELT(res, 8, resOR);

	for (int i = 0; i < realisation.steps; i++) {
		REAL(resT)[i] = realisation.Time[i];
		INTEGER(resYS)[i] = realisation.Y[i].S;
		INTEGER(resYE)[i] = realisation.Y[i].E;
		INTEGER(resYI)[i] = realisation.Y[i].I;
		INTEGER(resYR)[i] = realisation.Y[i].R;
		INTEGER(resOS)[i] = realisation.O[i].S;
		INTEGER(resOE)[i] = realisation.O[i].E;
		INTEGER(resOI)[i] = realisation.O[i].I;
		INTEGER(resOR)[i] = realisation.O[i].R;
	}

	UNPROTECT(10);

	/* Free our copy of results. */
	freeresult(realisation);


	return res;
}


static int binom(int, double);
static int poisson(double);

void freeresult(struct simresult res) {
	free(res.Time);
	free(res.Y);
	free(res.O);
}

struct simresult
seir_model_erlang(double t_phase, double time, double dt, const struct compartments *Y, const struct compartments *O, const double mean_holding_times[2], const double beta[2][2][2], const struct subcompartments *SYS_Y, const struct subcompartments *SYS_O) {
	int steps = 1;

	/* This summary of compartments will be our results. */
	struct compartments *Ytmp, *Otmp;
	Ytmp = malloc(steps*sizeof(*Ytmp));
	Otmp = malloc(steps*sizeof(*Otmp));
	assert(Ytmp != NULL && Otmp != NULL);
	Ytmp[0] = *Y;
	Otmp[0] = *O;

	int total_Y, total_O;
	total_Y = Y->S + Y->E + Y->I + Y->R;
	total_O = O->S + O->E + O->I + O->R;

	/* actual matrix to calculate with sub-compartments */
	struct subcompartments Ytmp_calc, Otmp_calc;
	Ytmp_calc = *SYS_Y;
	Otmp_calc = *SYS_O;

	/* Start time; */
	double *Time;
	Time = malloc(steps*sizeof(*Time));
	assert(Time != NULL);
	Time[0] = time;

	/* Check that what was passed in was consistent */
	struct compartments check;
	check.S = Ytmp_calc.S;
	check.E = Ytmp_calc.E[0];
	for (int j = 1; j < E_length; j++)
		check.E += Ytmp_calc.E[j];
	check.I = Ytmp_calc.I[0];
	for (int j = 1; j < I_length; j++)
		check.I += Ytmp_calc.I[j];
	check.R = Ytmp_calc.R;
	assert(check.S == Ytmp[0].S);
	assert(check.E == Ytmp[0].E);
	assert(check.I == Ytmp[0].I);
	assert(check.R == Ytmp[0].R);

	check.S = Otmp_calc.S;
	check.E = Otmp_calc.E[0];
	for (int j = 1; j < E_length; j++)
		check.E += Otmp_calc.E[j];
	check.I = Otmp_calc.I[0];
	for (int j = 1; j < I_length; j++)
		check.I += Otmp_calc.I[j];
	check.R = Otmp_calc.R;
	assert(check.S == Otmp[0].S);
	assert(check.E == Otmp[0].E);
	assert(check.I == Otmp[0].I);
	assert(check.R == Otmp[0].R);

	/* probability of E->I and I->R */
	double pE[E_length], pI[I_length];
	for (int j = 0; j < E_length; j++)
		pE[j] = 1-exp(-dt*E_length/mean_holding_times[0]);
	for (int j = 0; j < I_length; j++)
		pI[j] = 1-exp(-dt*I_length/mean_holding_times[1]);


	/* Run the simulations */
	int i;
	for (i = 0; Time[i] < t_phase && Ytmp[i].E + Ytmp[i].I + Otmp[i].E + Otmp[i].I > 0; i++) {
		int YEmovers[E_length], YImovers[I_length], Ytotal_infected;
		int OEmovers[E_length], OImovers[I_length], Ototal_infected;

		/* calculate number of people needs to be moved E -> I and I -> R */
		for (int j = 0; j < E_length; j++) {
			YEmovers[j] = binom(Ytmp_calc.E[j], pE[j]);
			OEmovers[j] = binom(Otmp_calc.E[j], pE[j]);
		}
		for (int j = 0; j < I_length; j++) {
			YImovers[j] = binom(Ytmp_calc.I[j], pI[j]);
			OImovers[j] = binom(Otmp_calc.I[j], pI[j]);
		}

		/*
		 * calculate sum of exposed and infectious people
		 * if it's greater than 0, there's a chance to infect susceptible people
		 */
		Ytotal_infected = 0;
		Ototal_infected = 0;
		if (Ytmp[i].E + Ytmp[i].I + Otmp[i].E + Otmp[i].I > 0) {
			int N_infected;

			/* First figure out Y group infections */
			N_infected = poisson(dt * Ytmp[i].S *
				    ((beta[0][0][0] * Ytmp[i].E +
				      beta[1][0][0] * Ytmp[i].I) / total_Y +
				     (beta[0][1][0] * Otmp[i].E +
				     beta[1][1][0] * Otmp[i].I) / total_O)
				);
			if (Ytmp[i].S >= N_infected)
				Ytotal_infected = N_infected;
			else
				Ytotal_infected = Ytmp[i].S;

			/* Now figure out O group infections */
			N_infected = poisson(dt * Otmp[i].S *
				    ((beta[0][0][1] * Ytmp[i].E +
				     beta[1][0][1] * Ytmp[i].I) / total_Y +
				     (beta[0][1][1] * Otmp[i].E +
				     beta[1][1][1] * Otmp[i].I) / total_O)
				);
			if (Otmp[i].S >= N_infected)
				Ototal_infected = N_infected;
			else
				Ototal_infected = Otmp[i].S;
		}

		/* update number of people in each compartment */
		Ytmp_calc.S += -Ytotal_infected;
		Ytmp_calc.E[0] += Ytotal_infected - YEmovers[0];
		for (int j = 1; j < E_length; j++)
			Ytmp_calc.E[j] += YEmovers[j-1] - YEmovers[j];
		Ytmp_calc.I[0] += YEmovers[E_length-1] - YImovers[0];
		for (int j = 1; j < I_length; j++)
			Ytmp_calc.I[j] += YImovers[j-1] - YImovers[j];
		Ytmp_calc.R += YImovers[I_length-1];

		Otmp_calc.S += -Ototal_infected;
		Otmp_calc.E[0] += Ototal_infected - OEmovers[0];
		for (int j = 1; j < E_length; j++)
			Otmp_calc.E[j] += OEmovers[j-1] - OEmovers[j];
		Otmp_calc.I[0] += OEmovers[E_length-1] - OImovers[0];
		for (int j = 1; j < I_length; j++)
			Otmp_calc.I[j] += OImovers[j-1] - OImovers[j];
		Otmp_calc.R += OImovers[I_length-1];
		
		/* Do we need more space? */
		if (i+1 >= steps) {
			int n;

			n = steps * 2 + 10;
			Otmp = realloc(Otmp, n*sizeof(*Otmp));
			Ytmp = realloc(Ytmp, n*sizeof(*Ytmp));
			assert(Otmp != NULL && Ytmp != NULL);
			Time = realloc(Time, n*sizeof(*Time));
			assert(Time != NULL);
			steps = n;
			/*printf("Expansion at time %lf to size %d (%d,%d,%d,%d)\n", Time[i], steps, Ntmp[i].S, Ntmp[i].E, Ntmp[i].I, Ntmp[i].R);*/
		}

		/* Store the results and move on */
		Otmp[i+1].S = Otmp_calc.S;
		Otmp[i+1].E = Otmp_calc.E[0];
		for (int j = 1; j < E_length; j++)
			Otmp[i+1].E += Otmp_calc.E[j];
		Otmp[i+1].I = Otmp_calc.I[0];
		for (int j = 1; j < I_length; j++)
			Otmp[i+1].I += Otmp_calc.I[j];
		Otmp[i+1].R = Otmp_calc.R;

		Ytmp[i+1].S = Ytmp_calc.S;
		Ytmp[i+1].E = Ytmp_calc.E[0];
		for (int j = 1; j < E_length; j++)
			Ytmp[i+1].E += Ytmp_calc.E[j];
		Ytmp[i+1].I = Ytmp_calc.I[0];
		for (int j = 1; j < I_length; j++)
			Ytmp[i+1].I += Ytmp_calc.I[j];
		Ytmp[i+1].R = Ytmp_calc.R;

		Time[i+1] = Time[i]+dt;
	}

	/* Fill out structure for returning the result */
	struct simresult res;
	res.steps = i+1;
	res.Time = Time;
	res.O = Otmp;
	res.Y = Ytmp;
	
	return res;
}


/*
 * Based on Numerical recipies code.
 */

static int
poisson(double xm) {
	static double sq,alxm,g,oldm=(-1.0);
	double em,t,y;

	if (xm < 12.0) {
		if (xm != oldm) {
			oldm=xm;
			g=exp(-xm);
		}
		em = -1;
		t=1.0;
		do {
			em += 1.0;
			t *= drand48();
		} while (t > g);
	} else {
		if (xm != oldm) {
			oldm=xm;
			sq=sqrt(2.0*xm);
			alxm=log(xm);
			g=xm*alxm-lgamma(xm+1.0);
		}
		do {
			do {
				y=tan(M_PI*drand48());
				em=sq*y+xm;
			} while (em < 0.0);
			em=floor(em);
			t=0.9*(1.0+y*y)*exp(em*alxm-lgamma(em+1.0)-g);
		} while (drand48() > t);
	}
	return em;
}

static int
binom(int n, double pp) {
	int j;
	static int nold=(-1);
	double am,em,g,angle,p,bnl,sq,t,y;
	static double pold=(-1.0),pc,plog,pclog,en,oldg;

	p=(pp <= 0.5 ? pp : 1.0-pp);
	am=n*p;
	if (n < 25) {
		bnl=0.0;
		for (j=1;j<=n;j++)
			if (drand48() < p) bnl += 1.0;
	} else if (am < 1.0) {
		g=exp(-am);
		t=1.0;
		for (j=0;j<=n;j++) {
			t *= drand48();
			if (t < g) break;
		}
		bnl=(j <= n ? j : n);
	} else {
		if (n != nold) {
			en=n;
			oldg=lgamma(en+1.0);
			nold=n;
		} if (p != pold) {
			pc=1.0-p;
			plog=log(p);
			pclog=log(pc);
			pold=p;
		}
		sq=sqrt(2.0*am*pc);
		do {
			do {
				angle=M_PI*drand48();
				y=tan(angle);
				em=sq*y+am;
			} while (em < 0.0 || em >= (en+1.0));
			em=floor(em);
			t=1.2*sq*(1.0+y*y)*exp(oldg-lgamma(em+1.0)
				-lgamma(en-em+1.0)+em*plog+(en-em)*pclog);
		} while (drand48() > t);
		bnl=em;
	}
	if (p != pp) bnl=n-bnl;
	return bnl;
}
