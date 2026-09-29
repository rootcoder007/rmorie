.scx_rpois <- function(mu, u) {
  k <- 0
  p <- exp(-mu)
  cc <- p
  while (cc < u && k < 100000) {
    k <- k + 1
    p <- p * mu / k
    cc <- cc + p
  }
  k
}

.scx_polish <- function(y, X, W, rho, lo = -0.99, hi = 0.99) {
  # Newton steps on the analytic profile score (y - mu)' (I - rho W)^-1 W Z beta
  n <- length(y)
  fit <- function(r) {
    Ai <- solve(diag(n) - r * W)
    Z <- Ai %*% X
    g <- .grv_glm(Z, y, "poisson")
    eta <- as.vector(Z %*% g$coefficients)
    list(s = sum((y - g$fitted) * as.vector(Ai %*% (W %*% eta))), g = g)
  }
  for (it in 1:6) {
    s0 <- fit(rho)$s
    h <- 1e-6
    ds <- (fit(rho + h)$s - fit(rho - h)$s) / (2 * h)
    if (!(ds < 0)) break
    nr <- rho - s0 / ds
    if (!(lo < nr && nr < hi)) break
    step <- abs(nr - rho)
    rho <- nr
    if (step < 1e-15) break
  }
  list(rho = rho, g = fit(rho)$g)
}

.scx_fit <- function(y, X, W) {
  .scx_polish(as.numeric(y), as.matrix(X) + 0, as.matrix(W) + 0, SarPoisson(y, X, W)$rho)
}

#' Spatial count model tests
#'
#' R arm of the Python modules \code{morie.fn.scdisp}, \code{scnblrt},
#' \code{scpboot} and \code{scpflx}. \code{Scdisp}: Cameron-Trivedi
#' regression test of equidispersion after a Poisson GLM (as
#' \code{AER::dispersiontest}; \code{trafo = NULL} for
#' \eqn{Var = (1 + \alpha)\mu}, \code{trafo = 2} for
#' \eqn{Var = \mu + \alpha\mu^2}), one-sided. \code{Scnblrt}: likelihood
#' ratio of NB against Poisson with the boundary (50:50 chi-bar-square)
#' p-value. \code{Scpboot}: parametric bootstrap of the spatial-lag Poisson
#' rho (\code{\link{SarPoisson}}, refined by Newton steps on the analytic
#' profile score), Poisson draws by inversion of Philox
#' uniforms, type-7 percentile interval. \code{Scpflx}: spatial-lag Poisson
#' with unit fixed effects on a panel stacked by period, block-diagonal
#' weights.
#'
#' @param y Counts.
#' @param X Design matrix (with intercept for \code{Scdisp} and
#'   \code{Scpboot}; time-varying regressors for \code{Scpflx}).
#' @param trafo NULL or 2.
#' @param ll_nb,ll_pois Log-likelihoods of the NB and the Poisson fit.
#' @param df Number of dispersion parameters.
#' @param W Spatial weights matrix.
#' @param B Bootstrap replicates.
#' @param seed Philox seed.
#' @param level Confidence level.
#' @param unit_id Unit label of every observation.
#' @return A list whose \code{statistic} is the headline value.
#' @references Cameron, A. C. and Trivedi, P. K. (1990). Regression-based
#'   tests for overdispersion in the Poisson model. Journal of Econometrics
#'   46, 347-364.
#'
#'   Self, S. G. and Liang, K.-Y. (1987). Asymptotic properties of maximum
#'   likelihood estimators and likelihood ratio tests under nonstandard
#'   conditions. Journal of the American Statistical Association 82,
#'   605-610.
#'
#'   Lambert, D. M., Brown, J. P. and Florax, R. J. G. M. (2010). A two-step
#'   estimator for a spatial lag model of counts. Regional Science and Urban
#'   Economics 40, 241-252.
#' @examples
#' X <- cbind(1, c(0.1, 0.4, 0.5, 0.9, 0.3, 0.7, 0.2, 0.8))
#' Scdisp(c(0, 3, 1, 9, 0, 6, 2, 1), X)$statistic
#' Scnblrt(-40, -42)$p_value
#' @export
Scdisp <- function(y, X, trafo = NULL) {
  y <- as.numeric(y)
  X <- as.matrix(X)
  n <- length(y)
  mu <- .grv_glm(X, y, "poisson")$fitted
  a <- ((y - mu)^2 - y) / mu
  if (is.null(trafo)) {
    alpha <- sum(a) / n
    se <- sqrt(sum((a - alpha)^2) / (n - 1) / n)
    disp <- 1 + alpha
  } else if (trafo == 2) {
    alpha <- sum(a * mu) / sum(mu^2)
    se <- sqrt(sum((a - alpha * mu)^2) / (n - 1) / sum(mu^2))
    disp <- alpha
  } else {
    stop("trafo must be NULL or 2")
  }
  z <- alpha / se
  list(statistic = z, p_value = stats::pnorm(z, lower.tail = FALSE), alpha = alpha, dispersion = disp)
}

#' @rdname Scdisp
#' @export
Scnblrt <- function(ll_nb, ll_pois, df = 1) {
  lr <- max(2 * (ll_nb - ll_pois), 0)
  p <- if (df == 1) 0.5 * stats::pchisq(lr, 1, lower.tail = FALSE) else stats::pchisq(lr, df, lower.tail = FALSE)
  list(statistic = lr, p_value = p, df = df)
}

#' @rdname Scdisp
#' @export
Scpboot <- function(y, X, W, B = 9, seed = 0, level = 0.95) {
  f <- .scx_fit(y, X, W)
  mu <- f$g$fitted
  n <- length(mu)
  u <- .morie_random_uniform(B * n, seed = seed)
  draws <- vapply(seq_len(B), function(b) {
    ys <- vapply(seq_len(n), function(i) .scx_rpois(mu[i], u[(b - 1) * n + i]), numeric(1))
    .scx_fit(ys, X, W)$rho
  }, numeric(1))
  a <- (1 - level) / 2
  list(statistic = f$rho, ci_lower = .sxd_q7(draws, a), ci_upper = .sxd_q7(draws, 1 - a),
       se_boot = if (B > 1) stats::sd(draws) else NaN, draws = draws)
}

#' @rdname Scdisp
#' @export
Scpflx <- function(y, X, W, unit_id) {
  y <- as.numeric(y)
  X <- as.matrix(X)
  W <- as.matrix(W)
  n <- length(y)
  N <- nrow(W)
  if (n %% N != 0 || nrow(X) != n || length(unit_id) != n) stop("y, X and unit_id must have N T rows, stacked by period")
  keep <- which(apply(X, 2, function(v) max(v) > min(v)))
  units <- unique(unit_id)
  Z <- cbind(X[, keep, drop = FALSE], outer(unit_id, units, "==") * 1)
  Wf <- kronecker(diag(n / N), W)
  f <- .scx_fit(y, Z, Wf)
  b <- f$g$coefficients
  list(statistic = f$rho, beta = b[seq_along(keep)], unit_effects = stats::setNames(b[-seq_along(keep)], units),
       loglik = f$g$loglik, fitted = f$g$fitted, periods = n / N)
}
