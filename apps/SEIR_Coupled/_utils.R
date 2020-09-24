# a function to change default y-axis label to x \times 10^y
scientific_10 <- function(x) {
  parse(text = gsub("e", "%*%10^", scales::scientific_format()(x)))
}