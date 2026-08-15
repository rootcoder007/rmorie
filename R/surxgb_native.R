.erf <- function(x) {
  # Abramowitz and Stegun approximation
  sign_x <- if (x >= 0) 1 else -1
  x <- abs(x)
  a1 <- 0.254829592
  a2 <- -0.284496736
  a3 <- 1.421413741
  a4 <- -1.453152027
  a5 <- 1.061405429
  p <- 0.3275911
  t <- 1 / (1 + p * x)
  y <- 1 - (((((a5*t + a4)*t) + a3)*t + a2)*t + a1)*t * exp(-x*x)
  return(sign_x * y)
}
