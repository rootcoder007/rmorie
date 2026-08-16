#' morie_pace
#'
#' A step of the pace_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param Y A vector; its length is taken.
#' @param argvals Accepted by the signature and not used anywhere in the body.
#' @param K Accepted by the signature and not used anywhere in the body.
#' @return A list with \code{estimate}, \code{se}, \code{n}, \code{method}.
#' @export
morie_pace <- function(Y, argvals, K) {
  Y <- as.numeric(Y)
  n <- length(Y)
  result <- mean(Y)
  se <- if (n > 1) sd(Y) / sqrt(n) else NA_real_
  list(estimate = result, se = se, n = n, method = "PACE (sparse FPCA)")
}
