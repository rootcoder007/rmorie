# SPDX-License-Identifier: AGPL-3.0-or-later

#' Ordinary least squares closed-form solution (R parity)
#'
#' Wraps \code{stats::lm} and returns coefficients plus classical OLS
#' standard errors.
#'
#' @param x Numeric matrix or vector of predictors.
#' @param y Numeric response vector.
#' @return Named list with \code{estimate} (intercept + slopes),
#'   \code{se}, \code{n}, \code{method}.
#' @references
#' Hastie, Tibshirani & Friedman, Elements of Statistical Learning (2009).
#' @examples
#' set.seed(1)
#' morie_linear_regression_ols(x = rnorm(50), y = rnorm(50))
#' @export
morie_linear_regression_ols <- function(x, y) {
  if (is.null(dim(x))) x <- matrix(x, ncol = 1)
  x <- as.matrix(x)
  y <- as.numeric(y)
  if (nrow(x) != length(y)) stop("x and y must have the same number of rows", call. = FALSE)
  if (anyNA(x) || anyNA(y)) stop("x and y must not contain missing values", call. = FALSE)
  constant <- vapply(seq_len(ncol(x)), function(j) length(unique(x[, j])) == 1L, TRUE)
  p <- ncol(x) + !any(constant)
  if (nrow(x) <= p) {
    stop(sprintf("at least %d rows are needed to fit %d coefficients", p + 1L, p), call. = FALSE)
  }
  df <- as.data.frame(x)
  df$.y <- y
  # a constant column supplied by the caller already is the intercept: fit without adding one
  fit <- if (any(constant)) stats::lm(.y ~ . + 0, data = df) else stats::lm(.y ~ ., data = df)
  s <- summary(fit)
  cf <- stats::coef(fit)
  est <- unname(cf)
  se <- rep(NA_real_, length(cf))
  se[match(rownames(s$coefficients), names(cf))] <- s$coefficients[, "Std. Error"]
  list(
    estimate = est,
    se       = se,
    n        = nrow(x),
    method   = "OLS via closed-form normal equations"
  )
}
