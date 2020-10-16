#
# The C code can be compiled from the commandline (not in R) with:
# 	R CMD SHLIB twoagesR.c
# The R code below will load C code and define a wrapper. It can be
# run with:
# 	result = twoages(1500000, 100, 0, 0, 1500000, 0, 0, 0, 1.1, 1.1, 1.1, 1.1)
# The result is an vector of results, the vectors are
# 	Time YS YE YI YR OS OE OI OR
#
dyn.load("twoagesR.so")
twoages <- function(YS, YE, YI, YR, OS, OE, OI, OR, YR0Y, YR0O, OR0Y, OR0O) {
  stopifnot(is.numeric(YS), is.numeric(YE), is.numeric(YI), is.numeric(YR))
  stopifnot(is.numeric(OS), is.numeric(OE), is.numeric(OI), is.numeric(OR))
  stopifnot(is.numeric(YR0Y), is.numeric(YR0O), is.numeric(OR0Y), is.numeric(OR0O))
  stopifnot(length(YS) == 1, length(YE) == 1, length(YI) == 1, length(YR) == 1)
  stopifnot(length(OS) == 1, length(OE) == 1, length(OI) == 1, length(OR) == 1)
  stopifnot(length(YR0Y) == 1, length(YR0O) == 1, length(OR0Y) == 1, length(OR0O) == 1)
  .Call("twoages", YS, YE, YI, YR, OS, OE, OI, OR, YR0Y, YR0O, OR0Y, OR0O)
}

# Run it with 
# result1 = twoages(4000000, 250, 250, 100000, 900000, 50, 50, 100000, 1.5, 0.3, 0.3, 0.6)
# plot(result1[[1]], result1[[4]], type = 'l')
# lines(result1[[1]], result1[[8]], col = 'red')
# result2 = twoages(4000000, 250, 250, 100000, 900000, 50, 50, 100000, 1.5, 0.3, 0.3, 5.0)
# plot(result2[[1]], result2[[4]], type = 'l')
# lines(result2[[1]], result2[[8]], col = 'red')

# Now try running it with differing values
# result1 = twoages(4000000, 250, 2000000, 100000, 900000, 50, 50, 100000, 8, 0.3, 0.3, 0.6)
# str(as.data.frame(result1))