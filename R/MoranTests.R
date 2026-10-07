#' Moran's I tests: moments, z-score and the residual variants
#'
#' `miexp` is the null expectation `E(I) = -1/(n - 1)`; `mivar` the null
#' variance from the weights constants `S0`, `S1`, `S2` under normality, or
#' under randomisation when the sample kurtosis `b2` is given; `mizval` the
#' standardised `z = (I - E(I)) / sqrt(Var(I))` with its normal p-value;
#' `minorm` combines the three.  `miorig` and `mirand` compute Moran's I
#' of `y` with the normality and randomisation moments
#' (`spdep::moran.test`), `mimc` the permutation test (`spdep::moran.mc`,
#' Philox-driven shuffles identical to the Python arm), `miols` the exact
#' OLS-residual test (`spdep::lm.morantest`), `miml` Moran's I of residuals
#' of a maximum-likelihood spatial model with randomisation moments, and
#' `miiv` the Anselin-Kelejian (1997) test for 2SLS residuals.
#'
#' @param n Number of observations.
#' @param S0 Sum of the weights.
#' @param S1 Half the sum of squared symmetrised weights.
#' @param S2 Sum of squared row plus column sums.
#' @param b2 Sample kurtosis; NULL selects the normality variance.
#' @param I Moran's I.
#' @param E_I Expectation of I.
#' @param Var_I Variance of I.
#' @param alternative One of "greater", "less" or "two.sided".
#' @param y Numeric vector of observations.
#' @param W Spatial weights matrix.
#' @param nsim Number of permutations.
#' @param seed Philox seed.
#' @param resid Residual vector.
#' @param X Design matrix of the OLS fit (default intercept only).
#' @param Z Regressor matrix of the 2SLS fit.
#' @param H Instrument matrix of the 2SLS fit.
#' @return A list with `statistic`, `p_value`, `expected`, `variance` and
#'   method-specific components (for `miiv`, `statistic` is the AK
#'   chi-square and `moran_i` the residual Moran's I).
#' @references Moran, P. A. P. (1950). Notes on continuous stochastic
#'   phenomena. Biometrika 37, 17-23. Cliff, A. D. and Ord, J. K. (1981).
#'   Spatial Processes: Models and Applications. Pion. Anselin, L. and
#'   Kelejian, H. H. (1997). Testing for spatial error autocorrelation in the
#'   presence of endogenous regressors. International Regional Science
#'   Review 20, 153-182.
#' @examples
#' W <- rbind(c(0, 1, 0, 0), c(1, 0, 1, 0), c(0, 1, 0, 1), c(0, 0, 1, 0))
#' minorm(0.2, 4, 6, 12, 40)$statistic
#' mirand(c(1, 2, 3, 4), W)$statistic
#' @export
miexp <- function(n) {
  .morie_arg(n, "i1")
  n <- as.integer(n)
  if (n < 2) stop("n must be at least 2")
  e <- -1 / (n - 1)
  list(statistic = e, expected = e, n = n)
}

#' @rdname miexp
#' @export
mivar <- function(n, S0, S1, S2, b2 = NULL) {
  n <- as.numeric(n)
  if (is.null(b2)) {
    v <- (n * n * S1 - n * S2 + 3 * S0 * S0) / (S0 * S0 * (n * n - 1)) - 1 / ((n - 1)^2)
    kind <- "normality"
  } else {
    num <- n * ((n * n - 3 * n + 3) * S1 - n * S2 + 3 * S0 * S0) -
      b2 * ((n * n - n) * S1 - 2 * n * S2 + 6 * S0 * S0)
    v <- num / ((n - 1) * (n - 2) * (n - 3) * S0 * S0) - 1 / ((n - 1)^2)
    kind <- "randomisation"
  }
  list(statistic = v, variance = v, assumption = kind)
}

#' @rdname miexp
#' @export
mizval <- function(I, E_I, Var_I, alternative = "greater") {
  if (!(Var_I > 0)) stop("Var_I must be positive")
  z <- (I - E_I) / sqrt(Var_I)
  p <- switch(alternative,
    greater = stats::pnorm(z, lower.tail = FALSE),
    less = stats::pnorm(z),
    two.sided = 2 * stats::pnorm(abs(z), lower.tail = FALSE),
    stop("alternative must be 'greater', 'less' or 'two.sided'")
  )
  list(statistic = z, p_value = p, expected = E_I, variance = Var_I, I = I, alternative = alternative)
}

#' @rdname miexp
#' @export
minorm <- function(I, n, S0, S1, S2, b2 = NULL, alternative = "greater") {
  e <- miexp(n)$statistic
  v <- mivar(n, S0, S1, S2, b2 = b2)$statistic
  r <- mizval(I, e, v, alternative = alternative)
  list(statistic = r$statistic, p_value = r$p_value, expected = e, variance = v, I = I,
       alternative = alternative, assumption = if (is.null(b2)) "normality" else "randomisation")
}

.mit_wrap <- function(r, alternative) {
  list(statistic = r$estimate, p_value = r$p_value, expected = r$expectation,
       variance = r$variance, z = r$statistic, alternative = alternative)
}

#' @rdname miexp
#' @export
miorig <- function(y, W, alternative = "greater") {
  .mit_wrap(morie_morans_i_test(as.numeric(y), W, randomisation = FALSE, alternative = alternative), alternative)
}

#' @rdname miexp
#' @export
mirand <- function(y, W, alternative = "greater") {
  .mit_wrap(morie_morans_i_test(as.numeric(y), W, randomisation = TRUE, alternative = alternative), alternative)
}

#' @rdname miexp
#' @export
mimc <- function(y, W, nsim = 99, seed = 0) {
  r <- MoranPermutationTest(y, W, nsim = nsim, seed = seed)
  list(statistic = r$statistic, p_value = r$p_value, simulated = r$simulated, nsim = nsim, seed = seed)
}

#' @rdname miexp
#' @export
miols <- function(resid, W, X = NULL) {
  e <- as.numeric(resid)
  if (is.null(X)) X <- matrix(1, length(e), 1)
  r <- ResidualMoran(e, as.matrix(X), W)
  list(statistic = r$I, p_value = r$pvalue, expected = r$expected, variance = r$variance, z = r$z)
}

#' @rdname miexp
#' @export
miml <- function(resid, W, alternative = "greater") {
  .mit_wrap(morie_morans_i_test(as.numeric(resid), W, randomisation = TRUE, alternative = alternative), alternative)
}

#' @rdname miexp
#' @export
miiv <- function(resid, W, Z, H) {
  e <- as.numeric(resid)
  W <- unname(as.matrix(W)) * 1
  Z <- unname(as.matrix(Z)) * 1
  H <- unname(as.matrix(H)) * 1
  n <- length(e)
  s0 <- sum(W)
  ete <- sum(e * e)
  mi <- n / s0 * sum(e * as.vector(W %*% e)) / ete
  sig2n <- ete / n
  tt <- sum(W * W) + sum(W * t(W))
  ZtH <- crossprod(Z, H)
  V <- solve(ZtH %*% solve(crossprod(H), t(ZtH)))
  g <- as.vector(crossprod(Z, crossprod(W, e)))
  phi2 <- (tt + 4 / sig2n * sum(g * (V %*% g))) / ((s0 / n)^2 * n)
  ak <- n * mi * mi / phi2
  list(statistic = ak, p_value = stats::pchisq(ak, 1, lower.tail = FALSE), moran_i = mi, phi2 = phi2, T = tt)
}
