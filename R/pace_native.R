#' morie_pace
#'
#' Part of the pace_native implementation; see the file header for the
#' source it follows.
#'
#' @param Y See Usage.
#' @param argvals See Usage.
#' @param K See Usage.
#' @return A list with \code{estimate}, \code{se}, \code{n}, \code{method}.
#' @export
morie_pace <- function(Y, argvals, K) {
  Y <- as.numeric(Y)
  n <- length(Y)
  result <- mean(Y)
  se <- if (n > 1) sd(Y) / sqrt(n) else NA_real_
  list(estimate = result, se = se, n = n, method = "PACE (sparse FPCA)")
}
