.dm_seg <- function(segments) {
  S <- as.matrix(segments)
  if (is.null(dim(segments))) S <- matrix(as.numeric(segments), 1)
  unname(S) * 1
}

#' Sample moments, errors and ensemble statistics
#'
#' `sample_mean` (1/N) sum x; `raw_moment` (1/N) sum x^k; `central_moment`
#' (1/N) sum (x - xbar)^k (population divisor); `skewness_coeff` mu3/mu2^1.5
#' and `kurtosis_coeff` mu4/mu2^2 - 3 (population, `e1071` type 1);
#' `rms_value` sqrt((1/N) sum x^2); `mean_squared_error` and
#' `root_mean_squared_error`; `ensemble_average` and the Bessel-corrected
#' `ensemble_variance` across synchronized sweeps (rows);
#' `esl_total_sum_squares` sum (y - ybar)^2; `popvar` the population
#' variance (two-pass) beside the computational identity mean(x^2) - xbar^2.
#'
#' @param x Numeric vector.
#' @param k Moment order.
#' @param x_hat Estimate of `x`.
#' @param segments Matrix of sweeps (one per row).
#' @param y Response vector.
#' @return Lists with `value` (or `estimate`/`variance`) and components as in
#'   the Python arm.
#' @references Rangayyan, R. M. (2015). Biomedical Signal Analysis, 2nd ed.
#'   Wiley-IEEE Press. Hastie, T., Tibshirani, R. and Friedman, J. (2009).
#'   The Elements of Statistical Learning, 2nd ed. Springer. Morin, D. J.
#'   (2016). Probability: For the Enthusiastic Beginner. CreateSpace.
#' @examples
#' x <- c(1, 2, 4, 7)
#' sample_mean(x)$value
#' skewness_coeff(x)$value
#' popvar(x)$variance
#' @export
sample_mean <- function(x) {
  x <- as.numeric(x)
  if (!length(x)) stop("x must be non-empty")
  list(name = "sample_mean", value = mean(x), mean = mean(x), n = length(x))
}

#' @rdname sample_mean
#' @export
raw_moment <- function(x, k = 1) {
  x <- as.numeric(x)
  v <- mean(x^k)
  list(name = "raw_moment", value = v, moment_order = k, raw_moment = v, n = length(x))
}

#' @rdname sample_mean
#' @export
central_moment <- function(x, k = 2) {
  x <- as.numeric(x)
  v <- mean((x - mean(x))^k)
  list(name = "central_moment", value = v, moment_order = k, central_moment = v, n = length(x))
}

#' @rdname sample_mean
#' @export
skewness_coeff <- function(x) {
  .morie_arg(x, "n")
  x <- as.numeric(x)
  d <- x - mean(x)
  m2 <- mean(d^2)
  s <- if (m2 == 0) 0 else mean(d^3) / m2^1.5
  list(name = "skewness_coeff", value = s, skewness = s, n = length(x))
}

#' @rdname sample_mean
#' @export
kurtosis_coeff <- function(x) {
  x <- as.numeric(x)
  d <- x - mean(x)
  m2 <- mean(d^2)
  k <- if (m2 == 0) 0 else mean(d^4) / m2^2 - 3
  list(name = "kurtosis_coeff", value = k, excess_kurtosis = k, n = length(x))
}

#' @rdname sample_mean
#' @export
rms_value <- function(x) {
  x <- as.numeric(x)
  r <- sqrt(mean(x^2))
  list(name = "rms_value", value = r, rms = r, n = length(x))
}

#' @rdname sample_mean
#' @export
mean_squared_error <- function(x, x_hat) {
  if (length(x) != length(x_hat) || !length(x)) stop("x and x_hat must be non-empty and of equal length")
  m <- mean((as.numeric(x) - as.numeric(x_hat))^2)
  list(name = "mean_squared_error", value = m, mse = m, n = length(x))
}

#' @rdname sample_mean
#' @export
root_mean_squared_error <- function(x, x_hat) {
  m <- mean_squared_error(x, x_hat)$mse
  list(name = "root_mean_squared_error", value = sqrt(m), rmse = sqrt(m), mse = m, n = length(x))
}

#' @rdname sample_mean
#' @export
ensemble_average <- function(segments) {
  S <- .dm_seg(segments)
  list(name = "ensemble_average", value = colMeans(S), M = nrow(S), N = ncol(S))
}

#' @rdname sample_mean
#' @export
ensemble_variance <- function(segments) {
  S <- .dm_seg(segments)
  if (nrow(S) < 2) stop("the ensemble variance needs at least two sweeps")
  v <- apply(S, 2, stats::var)
  list(name = "ensemble_variance", value = v, M = nrow(S), N = ncol(S), mean_var = mean(v))
}

#' @rdname sample_mean
#' @export
esl_total_sum_squares <- function(y) {
  y <- as.numeric(y)
  if (!length(y)) stop("the total sum of squares needs at least one observation.")
  tss <- sum((y - mean(y))^2)
  list(estimate = tss, mean = mean(y), n = length(y), is_degenerate = tss == 0,
       method = "TSS = sum (y_i - y_bar)^2")
}

#' @rdname sample_mean
#' @export
popvar <- function(x) {
  x <- as.numeric(x)
  if (!length(x)) stop("x must be non-empty")
  lhs <- mean((x - mean(x))^2)
  rhs <- mean(x^2) - mean(x)^2
  list(variance = lhs, n = length(x), lhs = lhs, rhs = rhs, identity_error = abs(lhs - rhs))
}
