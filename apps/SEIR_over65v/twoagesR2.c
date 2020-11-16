/*
 * Original Authors: Ken Duffy, HoChan Cheon, rewritten in C by David Malone.
 * An approximate SEIR epidemic model with sub-compartments in E and I
 * Adjusted for two population groups by David Malone and given an R interface.
 * Add vaccination code, as discussed with Andrew Parnel and Ken Duffy.
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
#define SV_length 5	/* subcompartments of SV - adjustable */

struct compartments {
	int SU, SV, SVNE, SNV, E, I, R, RV;
};

struct subcompartments {
	int SU, SV[SV_length], SVNE, SNV, E[E_length], I[I_length], R, RV;
};

struct simresult {
	int steps;
	double *Time;
	struct compartments *O, *Y;
};

struct simresult seir_model_erlang(double , double , double , const struct compartments *, const struct compartments *, const double mean_holding_times[2], double, const double beta[2][2][2], const struct subcompartments *S, const struct subcompartments *, int *, int , int *, int , double);

void freeresult(struct simresult res);

SEXP twoages2(SEXP YSU, SEXP YSNV, SEXP YE, SEXP YI, SEXP YR, SEXP OSU, SEXP OSNV, SEXP OE, SEXP OI, SEXP OR, SEXP YR0Y, SEXP YR0O, SEXP OR0Y, SEXP OR0O, SEXP Yvac, SEXP Ovac, SEXP Veff) {
	struct compartments Y, O;
	double R0[2][2], veff;
	static int initialised = 0;

	Y.SU = asInteger(YSU);
	Y.SV = 0;
	Y.SVNE = 0;
	Y.SNV = asInteger(YSNV);
	Y.E = asInteger(YE);
	Y.I = asInteger(YI);
	Y.R = asInteger(YR);
	Y.RV = 0;
	O.SU = asInteger(OSU);
	O.SV = 0;
	O.SVNE = 0;
	O.SNV = asInteger(OSNV);
	O.E = asInteger(OE);
	O.I = asInteger(OI);
	O.R = asInteger(OR);
	O.RV = 0;
	R0[0][0] = asReal(YR0Y);
	R0[0][1] = asReal(YR0O);
	R0[1][0] = asReal(OR0Y);
	R0[1][1] = asReal(OR0O);
	veff = asReal(Veff);

	if (!initialised) {
		srand48(time(NULL));
		initialised = 1;
	}

	/*
	 * by default, assign initial Exposed and Infectious people
	 * roughly evenly over SV_i, E_i and I_i, respectively
	 */
	struct subcompartments SYS_Y, SYS_O;
	SYS_Y.SU = Y.SU;
	SYS_Y.SV[0] = Y.SV/SV_length + Y.SV%SV_length;
	for (int j = 1; j < SV_length; j++)
		SYS_Y.SV[j] = Y.SV/SV_length;
	SYS_Y.SVNE = Y.SVNE;
	SYS_Y.SNV = Y.SNV;
	SYS_Y.E[0] = Y.E/E_length + Y.E%E_length;
	for (int j = 1; j < E_length; j++)
		SYS_Y.E[j] = Y.E/E_length;
	SYS_Y.I[0] = Y.I/I_length + Y.I%I_length;
	for (int j = 1; j < I_length; j++)
		SYS_Y.I[j] = Y.I/I_length;
	SYS_Y.R = Y.R;
	SYS_Y.RV = Y.RV;
	SYS_O.SU = O.SU;
	SYS_O.SV[0] = O.SV/SV_length + O.SV%SV_length;
	for (int j = 1; j < SV_length; j++)
		SYS_O.SV[j] = O.SV/SV_length;
	SYS_O.SVNE = O.SVNE;
	SYS_O.SNV = O.SNV;
	SYS_O.E[0] = O.E/E_length + O.E%E_length;
	for (int j = 1; j < E_length; j++)
		SYS_O.E[j] = O.E/E_length;
	SYS_O.I[0] = O.I/I_length + O.I%I_length;
	for (int j = 1; j < I_length; j++)
		SYS_O.I[j] = O.I/I_length;
	SYS_O.R = O.R;
	SYS_O.RV = O.RV;

	/* Epidemic model setup: parameters */
	int N_phases;

	N_phases = 1; /* number of phases (e.g. intervention at t=t*) */

	/* equal mean holding times for E and I compartments (mean(E+I) = 14days) */
	double mean_holding_times[2], vacc_holding_time;
	mean_holding_times[0] = 6.6; /* mean holding times at compartment E */
	mean_holding_times[1] = 7.4; /* mean holding times at compartment I */
	vacc_holding_time = 14.0; /* mean holding time for SV compartment */

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
			realisation = seir_model_erlang(t_phase, t, dt, &Y, &O, mean_holding_times, vacc_holding_time, beta, &SYS_Y, &SYS_O, INTEGER(Yvac), length(Yvac), INTEGER(Ovac), length(Ovac), veff);
		} else {
			/* TODO: pass results from the first phase with different infection parameters */
			assert(0);
		}
	}

	/* Put together results for R */
	SEXP res = PROTECT(allocVector(VECSXP, 17));

	SEXP resT = PROTECT(allocVector(REALSXP, realisation.steps));
	SEXP resYSU = PROTECT(allocVector(INTSXP, realisation.steps));
	SEXP resYSV = PROTECT(allocVector(INTSXP, realisation.steps));
	SEXP resYSVNE = PROTECT(allocVector(INTSXP, realisation.steps));
	SEXP resYSNV = PROTECT(allocVector(INTSXP, realisation.steps));
	SEXP resYE = PROTECT(allocVector(INTSXP, realisation.steps));
	SEXP resYI = PROTECT(allocVector(INTSXP, realisation.steps));
	SEXP resYR = PROTECT(allocVector(INTSXP, realisation.steps));
	SEXP resYRV = PROTECT(allocVector(INTSXP, realisation.steps));
	SEXP resOSU = PROTECT(allocVector(INTSXP, realisation.steps));
	SEXP resOSV = PROTECT(allocVector(INTSXP, realisation.steps));
	SEXP resOSVNE = PROTECT(allocVector(INTSXP, realisation.steps));
	SEXP resOSNV = PROTECT(allocVector(INTSXP, realisation.steps));
	SEXP resOE = PROTECT(allocVector(INTSXP, realisation.steps));
	SEXP resOI = PROTECT(allocVector(INTSXP, realisation.steps));
	SEXP resOR = PROTECT(allocVector(INTSXP, realisation.steps));
	SEXP resORV = PROTECT(allocVector(INTSXP, realisation.steps));

	SET_VECTOR_ELT(res, 0, resT);
	SET_VECTOR_ELT(res, 1, resYSU);
	SET_VECTOR_ELT(res, 2, resYSV);
	SET_VECTOR_ELT(res, 3, resYSVNE);
	SET_VECTOR_ELT(res, 4, resYSNV);
	SET_VECTOR_ELT(res, 5, resYE);
	SET_VECTOR_ELT(res, 6, resYI);
	SET_VECTOR_ELT(res, 7, resYR);
	SET_VECTOR_ELT(res, 8, resYRV);
	SET_VECTOR_ELT(res, 9, resOSU);
	SET_VECTOR_ELT(res, 10, resOSV);
	SET_VECTOR_ELT(res, 11, resOSVNE);
	SET_VECTOR_ELT(res, 12, resOSNV);
	SET_VECTOR_ELT(res, 13, resOE);
	SET_VECTOR_ELT(res, 14, resOI);
	SET_VECTOR_ELT(res, 15, resOR);
	SET_VECTOR_ELT(res, 16, resORV);

	for (int i = 0; i < realisation.steps; i++) {
		REAL(resT)[i] = realisation.Time[i];
		INTEGER(resYSU)[i] = realisation.Y[i].SU;
		INTEGER(resYSV)[i] = realisation.Y[i].SV;
		INTEGER(resYSVNE)[i] = realisation.Y[i].SVNE;
		INTEGER(resYSNV)[i] = realisation.Y[i].SNV;
		INTEGER(resYE)[i] = realisation.Y[i].E;
		INTEGER(resYI)[i] = realisation.Y[i].I;
		INTEGER(resYR)[i] = realisation.Y[i].R;
		INTEGER(resYRV)[i] = realisation.Y[i].RV;
		INTEGER(resOSU)[i] = realisation.O[i].SU;
		INTEGER(resOSV)[i] = realisation.O[i].SV;
		INTEGER(resOSVNE)[i] = realisation.O[i].SVNE;
		INTEGER(resOSNV)[i] = realisation.O[i].SNV;
		INTEGER(resOE)[i] = realisation.O[i].E;
		INTEGER(resOI)[i] = realisation.O[i].I;
		INTEGER(resOR)[i] = realisation.O[i].R;
		INTEGER(resORV)[i] = realisation.O[i].RV;
	}

	UNPROTECT(18);

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
seir_model_erlang(double t_phase, double time, double dt, const struct compartments *Y, const struct compartments *O, const double mean_holding_times[2], double vacc_holding_time, const double beta[2][2][2], const struct subcompartments *SYS_Y, const struct subcompartments *SYS_O, int *Yv, int Yvl, int *Ov, int Ovl, double veff) {
	int steps = 1;

	/* This summary of compartments will be our results. */
	struct compartments *Ytmp, *Otmp;
	Ytmp = malloc(steps*sizeof(*Ytmp));
	Otmp = malloc(steps*sizeof(*Otmp));
	assert(Ytmp != NULL && Otmp != NULL);
	Ytmp[0] = *Y;
	Otmp[0] = *O;

	int total_Y, total_O;
	total_Y = Y->SU + Y->SV + Y->SVNE + Y->SNV + Y->E + Y->I + Y->R + Y->RV;
	total_O = O->SU + O->SV + O->SVNE + O->SNV + O->E + O->I + O->R + O->RV;

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
	check.SU = Ytmp_calc.SU;
	check.SV = Ytmp_calc.SV[0];
	for (int j = 1; j < SV_length; j++)
		check.SV += Ytmp_calc.SV[j];
	check.SVNE = Ytmp_calc.SVNE;
	check.SNV = Ytmp_calc.SNV;
	check.E = Ytmp_calc.E[0];
	for (int j = 1; j < E_length; j++)
		check.E += Ytmp_calc.E[j];
	check.I = Ytmp_calc.I[0];
	for (int j = 1; j < I_length; j++)
		check.I += Ytmp_calc.I[j];
	check.R = Ytmp_calc.R;
	check.RV = Ytmp_calc.RV;
	assert(check.SU == Ytmp[0].SU);
	assert(check.SV == Ytmp[0].SV);
	assert(check.SVNE == Ytmp[0].SVNE);
	assert(check.SNV == Ytmp[0].SNV);
	assert(check.E == Ytmp[0].E);
	assert(check.I == Ytmp[0].I);
	assert(check.R == Ytmp[0].R);
	assert(check.RV == Ytmp[0].RV);

	check.SU = Otmp_calc.SU;
	check.SV = Otmp_calc.SV[0];
	for (int j = 1; j < SV_length; j++)
		check.SV += Otmp_calc.SV[j];
	check.SVNE = Otmp_calc.SVNE;
	check.SNV = Otmp_calc.SNV;
	check.E = Otmp_calc.E[0];
	for (int j = 1; j < E_length; j++)
		check.E += Otmp_calc.E[j];
	check.I = Otmp_calc.I[0];
	for (int j = 1; j < I_length; j++)
		check.I += Otmp_calc.I[j];
	check.R = Otmp_calc.R;
	check.RV = Otmp_calc.RV;
	assert(check.SU == Otmp[0].SU);
	assert(check.SV == Otmp[0].SV);
	assert(check.SVNE == Otmp[0].SVNE);
	assert(check.SNV == Otmp[0].SNV);
	assert(check.E == Otmp[0].E);
	assert(check.I == Otmp[0].I);
	assert(check.R == Otmp[0].R);
	assert(check.RV == Otmp[0].RV);

	/* probability of E->I and I->R */
	double pE[E_length], pI[I_length], pSV[SV_length];
	for (int j = 0; j < E_length; j++)
		pE[j] = 1-exp(-dt*E_length/mean_holding_times[0]);
	for (int j = 0; j < I_length; j++)
		pI[j] = 1-exp(-dt*I_length/mean_holding_times[1]);
	for (int j = 0; j < SV_length; j++)
		pSV[j] = 1-exp(-dt*SV_length/vacc_holding_time);


	/* Run the simulations */
	int i;
	for (i = 0; Time[i] < t_phase && Ytmp[i].E + Ytmp[i].I + Otmp[i].E + Otmp[i].I > 0; i++) {
		int YEmovers[E_length], YImovers[I_length];
		int OEmovers[E_length], OImovers[I_length];
		int YSVmovers[SV_length], OSVmovers[SV_length];
		int YSU_infected, YSV_infected[SV_length], YSVNE_infected, YSNV_infected;
		int OSU_infected, OSV_infected[SV_length], OSVNE_infected, OSNV_infected;

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
		 * Macros for dealing with infections in Y and O groups.
		 */
#define Y_do_group(SUCEPTABLE, INFECTED)  do { \
		int N_infected; \
		N_infected = poisson(dt * SUCEPTABLE * \
			    ((beta[0][0][0] * Ytmp[i].E + \
			      beta[1][0][0] * Ytmp[i].I) / total_Y + \
			     (beta[0][1][0] * Otmp[i].E + \
			      beta[1][1][0] * Otmp[i].I) / total_O) \
			); \
		if (SUCEPTABLE >= N_infected) \
			INFECTED = N_infected; \
		else \
			INFECTED = SUCEPTABLE; \
		} while (0)

#define O_do_group(SUCEPTABLE, INFECTED)  do { \
		int N_infected; \
		N_infected = poisson(dt * SUCEPTABLE * \
			    ((beta[0][0][1] * Ytmp[i].E + \
			      beta[1][0][1] * Ytmp[i].I) / total_Y + \
			     (beta[0][1][1] * Otmp[i].E + \
			      beta[1][1][1] * Otmp[i].I) / total_O) \
			); \
		if (SUCEPTABLE >= N_infected) \
			INFECTED = N_infected; \
		else \
			INFECTED = SUCEPTABLE; \
		} while (0)

		/*
		 * calculate sum of exposed and infectious people
		 * if it's greater than 0, there's a chance to infect susceptible people
		 */
		YSU_infected = 0;
		for (int j = 0; j < SV_length; j++)
			YSV_infected[j] = 0;
		YSVNE_infected = 0;
		YSNV_infected = 0;
		OSU_infected = 0;
		for (int j = 0; j < SV_length; j++)
			OSV_infected[j] = 0;
		OSVNE_infected = 0;
		OSNV_infected = 0;
		if (Ytmp[i].E + Ytmp[i].I + Otmp[i].E + Otmp[i].I > 0) {
			Y_do_group(Ytmp_calc.SU, YSU_infected);
			for (int j = 0; j < SV_length; j++)
				Y_do_group(Ytmp_calc.SV[j], YSV_infected[j]);
			Y_do_group(Ytmp_calc.SVNE, YSVNE_infected);
			Y_do_group(Ytmp_calc.SNV, YSNV_infected);
				
			O_do_group(Otmp_calc.SU, OSU_infected);
			for (int j = 0; j < SV_length; j++)
				O_do_group(Otmp_calc.SV[j], OSV_infected[j]);
			O_do_group(Otmp_calc.SVNE, OSVNE_infected);
			O_do_group(Otmp_calc.SNV, OSNV_infected);
		}

		/*
		 * update number of people in each compartment for infection.
		 */
		Ytmp_calc.SU += -YSU_infected;
		for (int j = 0; j < SV_length; j++) {
			Ytmp_calc.SV[j] += -YSV_infected[j];
			Ytmp_calc.E[0] += YSV_infected[j];
		}
		Ytmp_calc.SVNE += -YSVNE_infected;
		Ytmp_calc.SNV += -YSNV_infected;
		Ytmp_calc.E[0] += YSU_infected + YSVNE_infected + YSNV_infected - YEmovers[0];
		for (int j = 1; j < E_length; j++)
			Ytmp_calc.E[j] += YEmovers[j-1] - YEmovers[j];
		Ytmp_calc.I[0] += YEmovers[E_length-1] - YImovers[0];
		for (int j = 1; j < I_length; j++)
			Ytmp_calc.I[j] += YImovers[j-1] - YImovers[j];
		Ytmp_calc.R += YImovers[I_length-1];

		Otmp_calc.SU += -OSU_infected;
		for (int j = 0; j < SV_length; j++) {
			Otmp_calc.SV[j] += -OSV_infected[j];
			Otmp_calc.E[0] += OSV_infected[j];
		}
		Otmp_calc.SVNE += -OSVNE_infected;
		Otmp_calc.SNV += -OSNV_infected;
		Otmp_calc.E[0] += OSU_infected + OSVNE_infected + OSNV_infected - OEmovers[0];
		for (int j = 1; j < E_length; j++)
			Otmp_calc.E[j] += OEmovers[j-1] - OEmovers[j];
		Otmp_calc.I[0] += OEmovers[E_length-1] - OImovers[0];
		for (int j = 1; j < I_length; j++)
			Otmp_calc.I[j] += OImovers[j-1] - OImovers[j];
		Otmp_calc.R += OImovers[I_length-1];

		/*
		 * Move on people in vaccine groups.
		 */
		for (int j = 0; j < SV_length; j++) {
			YSVmovers[j] = binom(Ytmp_calc.SV[j], pSV[j]);
			OSVmovers[j] = binom(Otmp_calc.SV[j], pSV[j]);
		}
		Ytmp_calc.SV[0] -= YSVmovers[0];
		Otmp_calc.SV[0] -= OSVmovers[0];
		for (int j = 1; j < SV_length; j++) {
			Ytmp_calc.SV[j] += YSVmovers[j-1] - YSVmovers[j];
			Otmp_calc.SV[j] += OSVmovers[j-1] - OSVmovers[j];
		}

		/*
		 * After vaccine period, move to immunised or ineffective grp.
		 */
		int effective;
		effective = binom(YEmovers[SV_length-1], veff);
		Ytmp_calc.RV += effective;
		Ytmp_calc.SVNE += YEmovers[SV_length-1] - effective;
		effective = binom(OEmovers[SV_length-1], veff);
		Otmp_calc.RV += effective;
		Otmp_calc.SVNE += OEmovers[SV_length-1] - effective;
		
		/*
		 * Vaccinate for some people in SU.
		 * We have seperate vaccine numbers for Y and O.
		 */
		if (i < Yvl) { /* There is vaccinations still on list */
			if (Ytmp_calc.SU >= Yv[i]) {
				Ytmp_calc.SV[0] += Yv[i];
				Ytmp_calc.SU -= Yv[i];
			} else {
				Ytmp_calc.SV[0] += Ytmp_calc.SU;
				Ytmp_calc.SU = 0;
			}
		}

		if (i < Ovl) { /* There is vaccinations still on list */
			if (Otmp_calc.SU >= Ov[i]) {
				Otmp_calc.SV[0] += Ov[i];
				Otmp_calc.SU -= Ov[i];
			} else {
				Otmp_calc.SV[0] += Otmp_calc.SU;
				Otmp_calc.SU = 0;
			}
		}

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
		Ytmp[i+1].SU = Ytmp_calc.SU;
		Ytmp[i+1].SV = Ytmp_calc.SV[0];
		for (int j = 1; j < SV_length; j++)
			Ytmp[i+1].SV += Ytmp_calc.SV[j];
		Ytmp[i+1].SVNE = Ytmp_calc.SVNE;
		Ytmp[i+1].SNV = Ytmp_calc.SNV;
		Ytmp[i+1].E = Ytmp_calc.E[0];
		for (int j = 1; j < E_length; j++)
			Ytmp[i+1].E += Ytmp_calc.E[j];
		Ytmp[i+1].I = Ytmp_calc.I[0];
		for (int j = 1; j < I_length; j++)
			Ytmp[i+1].I += Ytmp_calc.I[j];
		Ytmp[i+1].R = Ytmp_calc.R;
		Ytmp[i+1].RV = Ytmp_calc.RV;

		Otmp[i+1].SU = Otmp_calc.SU;
		Otmp[i+1].SV = Otmp_calc.SV[0];
		for (int j = 1; j < SV_length; j++)
			Otmp[i+1].SV += Otmp_calc.SV[j];
		Otmp[i+1].SVNE = Otmp_calc.SVNE;
		Otmp[i+1].SNV = Otmp_calc.SNV;
		Otmp[i+1].E = Otmp_calc.E[0];
		for (int j = 1; j < E_length; j++)
			Otmp[i+1].E += Otmp_calc.E[j];
		Otmp[i+1].I = Otmp_calc.I[0];
		for (int j = 1; j < I_length; j++)
			Otmp[i+1].I += Otmp_calc.I[j];
		Otmp[i+1].R = Otmp_calc.R;
		Otmp[i+1].RV = Otmp_calc.RV;

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
