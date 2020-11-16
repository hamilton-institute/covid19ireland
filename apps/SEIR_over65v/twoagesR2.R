#
# The C code can be compiled from the commandline (not in R) with:
# 	R CMD SHLIB twoagesR2.c
# The R code below will load C code and define a wrapper. It can be
# run with:
# 	result = twoagesv2(1000000, 500000, 100, 0, 0, 1000000, 500000, 0, 0, 0, 1.1, 1.1, 1.1, 1.1, rep(20000,100), rep(20000,100), 0.9)
# The result is an vector of results, the vectors are
# 	Time YSU YSV YSVNE YSNV YE YI YR YRV OSU OSV OSVNE OSNV OE OI OR ORV
#
dyn.load("twoagesR2.so")
twoagesv2 <- function(YSU, YSNV, YE, YI, YR, OSU, OSNV, OE, OI, OR, YR0Y, YR0O, OR0Y, OR0O, Yvac, Ovac, Veff) {
  stopifnot(is.numeric(YSU), is.numeric(YSNV), is.numeric(YE), is.numeric(YI), is.numeric(YR))
  stopifnot(is.numeric(OSU), is.numeric(OSNV), is.numeric(OE), is.numeric(OI), is.numeric(OR))
  stopifnot(is.numeric(Yvac), is.numeric(Ovac), is.numeric(Veff))
  stopifnot(length(YSU) == 1, length(YSNV) == 1, length(YE) == 1, length(YI) == 1, length(YR) == 1)
  stopifnot(length(OSU) == 1, length(OSNV) == 1, length(OE) == 1, length(OI) == 1, length(OR) == 1)
  stopifnot(length(YR0Y) == 1, length(YR0O) == 1, length(OR0Y) == 1, length(OR0O) == 1)
  stopifnot(length(Yvac) >= 1, length(Ovac) >= 1)
  .Call("twoages2", YSU, YSNV, YE, YI, YR, OSU, OSNV, OE, OI, OR, YR0Y, YR0O, OR0Y, OR0O, as.integer(Yvac), as.integer(Ovac), Veff)
}
