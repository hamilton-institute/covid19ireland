#
# The C code can be compiled from the commandline (not in R) with:
# 	R CMD SHLIB twoagesR.c
# The R code below will load C code and define a wrapper. It can be
# run with:
# 	result = twoagesv(1500000, 100, 0, 0, 1500000, 0, 0, 0, 1.1, 1.1, 1.1, 1.1, rep(20000,100), rep(20000,100))
# The result is an vector of results, the vectors are
# 	Time YS YE YI YR OS OE OI OR
#
dyn.load("twoagesR.so")
twoagesv <- function(YS, YE, YI, YR, OS, OE, OI, OR, YR0Y, YR0O, OR0Y, OR0O, Yvac, Ovac) {
  stopifnot(is.numeric(YS), is.numeric(YE), is.numeric(YI), is.numeric(YR))
  stopifnot(is.numeric(OS), is.numeric(OE), is.numeric(OI), is.numeric(OR))
  stopifnot(is.numeric(YR0Y), is.numeric(YR0O), is.numeric(OR0Y), is.numeric(OR0O))
  stopifnot(is.numeric(Yvac), is.numeric(Ovac))
  stopifnot(length(YS) == 1, length(YE) == 1, length(YI) == 1, length(YR) == 1)
  stopifnot(length(OS) == 1, length(OE) == 1, length(OI) == 1, length(OR) == 1)
  stopifnot(length(YR0Y) == 1, length(YR0O) == 1, length(OR0Y) == 1, length(OR0O) == 1)
  stopifnot(length(Yvac) >= 1, length(Ovac) >= 1)
  .Call("twoages", YS, YE, YI, YR, OS, OE, OI, OR, YR0Y, YR0O, OR0Y, OR0O, as.integer(Yvac), as.integer(Ovac))
}

# Run it with 
# result1 = twoagesv(4000000, 250, 250, 100000, 900000, 50, 50, 100000, 1.5, 0.3, 0.3, 0.6, rep(20000,100), rep(20000,100))
# plot(result1[[1]], result1[[4]], type = 'l')
# lines(result1[[1]], result1[[8]], col = 'red')
# result2 = twoagesv(4000000, 250, 250, 100000, 900000, 50, 50, 100000, 1.5, 0.3, 0.3, 5.0, rep(20000,100), rep(20000,100))
# plot(result2[[1]], result2[[4]], type = 'l')
# lines(result2[[1]], result2[[8]], col = 'red')

# Now try running it with differing values
# result1 = twoagesv(4000000, 250, 2000000, 100000, 900000, 50, 50, 100000, 8, 0.3, 0.3, 0.6, rep(20000,100), rep(20000,100))
# str(as.data.frame(result1))
