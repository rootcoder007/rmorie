# SPDX-License-Identifier: AGPL-3.0-or-later

#' Farrington flexible algorithm
#'
#' Formula: a quasi-Poisson GLM with overdispersion fitted to the
#' reference baseline -- the same calendar window in each of the previous
#' \code{baseline_years} years, plus/minus \code{reference_window} time
#' units -- and a 2/3-power upper threshold at the current point.
#'
#' With log mu_i = alpha + beta t_i, dispersion
#' phi = max(1, sum w_i (y_i - mu_i)^2 / mu_i / (n - p)), prediction mu0
#' and its response-scale standard error se0, the threshold is
#' (Farrington et al 1996 sec. 2.3)
#' \preformatted{
#'   tau = phi + se0^2 / mu0
#'   se  = sqrt(4/9 mu0^(1/3) tau)
#'   U   = (mu0^(2/3) + z_{1-alpha/2} se)^(3/2)
#' }
#' and an alarm is raised when the current count exceeds U.  When
#' \code{reweight} is set the fit is repeated with Farrington weights
#' from standardised Anscombe residuals
#' a_i = 1.5 (y^(2/3) mu^(-1/6) - mu^(1/2)) / sqrt(phi (1 - h_i)),
#' down-weighting past outbreaks by a_i^-2.  Following Noufaily et al
#' (2013) the trend term is retained whether or not it is significant.
#'
#' @param counts Full count series; the LAST element is the point tested.
#' @param baseline_years Number of previous years b in the baseline.
#' @param reference_window Half-width w around each anniversary.
#' @param period Observations per year (52 for weekly data).
#' @param alpha Two-sided level for the threshold.
#' @param reweight Apply the Anscombe-residual re-weighting pass.
#' @param trend Include the linear time trend.
#' @return List with \code{estimate}, \code{observed}, \code{expected},
#'   \code{threshold}, \code{alarm}, \code{phi}, \code{phi_raw},
#'   \code{trend_coef},
#'   \code{score}, \code{nbaseline}, \code{n}, \code{method}.
#' @references Farrington, Andrews, Beale & Catchpole (1996), JRSS-A
#'   159(3):547-563, doi:10.2307/2983331; Noufaily, Enki, Farrington,
#'   Garthwaite, Andrews & Charlett (2013), Statistics in Medicine
#'   32(7):1206-1222, doi:10.1002/sim.5595.
#' @export
Farsig <- function(counts, baseline_years = 5, reference_window = 3,
                   period = 52, alpha = 0.005, reweight = TRUE, trend = TRUE) {
  .irls <- function(X, y, w, iters = 60L, tol = 1e-12) {
    n <- length(y); p <- ncol(X)
    b <- numeric(p)
    m <- sum(y * w) / max(sum(w), 1e-300)
    b[1] <- log(if (m > 0) m else 0.5)
    for (.k in seq_len(iters)) {
      eta <- as.numeric(X %*% b)
      mu <- exp(eta)
      A <- matrix(0, p, p); rhs <- numeric(p)
      for (i in seq_len(n)) {
        wi <- w[i] * mu[i]
        z <- if (mu[i] > 0) eta[i] + (y[i] - mu[i]) / mu[i] else eta[i]
        for (a in seq_len(p)) {
          for (cc in seq_len(p)) A[a, cc] <- A[a, cc] + wi * X[i, a] * X[i, cc]
          rhs[a] <- rhs[a] + wi * X[i, a] * z
        }
      }
      nb <- .s03cholsolve(A, rhs)
      d <- max(abs(nb - b))
      b <- nb
      if (d < tol) break
    }
    eta <- as.numeric(X %*% b); mu <- exp(eta)
    A <- matrix(0, p, p)
    for (a in seq_len(p)) for (cc in seq_len(p))
      A[a, cc] <- sum(w * mu * X[, a] * X[, cc])
    inv <- lapply(seq_len(p), function(cc) {
      e <- numeric(p); e[cc] <- 1; .s03cholsolve(A, e)
    })
    list(b = b, mu = mu, inv = inv)
  }
  y <- as.numeric(counts)
  n <- length(y)
  if (n == 0L) stop("empty input: counts has no observations")
  if (any(y < 0)) stop("counts must be non-negative")
  b <- as.integer(baseline_years); w <- as.integer(reference_window)
  per <- as.integer(period)
  if (b < 1L) stop("baseline_years must be positive")
  if (w < 0L) stop("reference_window must be non-negative")
  if (per < 1L) stop("period must be positive")
  a <- as.numeric(alpha)
  if (!(a > 0 && a < 1)) stop("alpha must lie in (0, 1)")
  trend <- isTRUE(as.logical(trend))
  t0 <- n - 1L
  idx <- integer(0)
  for (j in seq_len(b)) {
    cc <- t0 - j * per
    for (d in (-w):w) {
      k <- cc + d
      if (k >= 0L && k < t0) idx <- c(idx, k)
    }
  }
  idx <- sort(unique(idx))
  nb <- length(idx)
  p <- if (trend) 2L else 1L
  if (nb < p + 1L) stop(sprintf("not enough baseline observations (%d)", nb))
  X <- if (trend) cbind(rep(1, nb), as.numeric(idx - t0)) else matrix(1, nb, 1L)
  yb <- y[idx + 1L]
  om <- rep(1, nb)
  .fit <- function(om) {
    f <- .irls(X, yb, om)
    raw <- sum(om * (yb - f$mu)^2 / f$mu) / (nb - p)
    phi <- if (raw > 1) raw else 1
    list(b = f$b, mu = f$mu, inv = f$inv, phi = phi, raw = raw)
  }
  f <- .fit(om)
  if (isTRUE(as.logical(reweight))) {
    hat <- numeric(nb)
    for (i in seq_len(nb)) {
      q <- 0
      for (r in seq_len(p)) {
        v <- 0
        for (cc in seq_len(p)) v <- v + f$inv[[cc]][r] * X[i, cc]
        q <- q + X[i, r] * v
      }
      hat[i] <- om[i] * f$mu[i] * q
    }
    s <- numeric(nb)
    for (i in seq_len(nb)) {
      an <- 1.5 * (yb[i]^(2 / 3) * f$mu[i]^(-1 / 6) - f$mu[i]^0.5)
      den <- f$phi * (1 - hat[i])
      s[i] <- if (den > 0) an / sqrt(den) else 0
    }
    den <- sum(ifelse(s > 1, s^(-2), 1))
    gam <- if (den > 0) nb / den else 1
    om <- ifelse(s > 1, gam * s^(-2), gam)
    f <- .fit(om)
  }
  x0 <- if (trend) c(1, 0) else 1
  mu0 <- exp(sum(x0 * f$b))
  q0 <- 0
  for (r in seq_len(p)) {
    v <- 0
    for (cc in seq_len(p)) v <- v + f$inv[[cc]][r] * x0[cc]
    q0 <- q0 + x0[r] * v
  }
  # predict.glm scales the standard error by the RAW (un-floored)
  # dispersion, which is what surveillance::algo.farrington consumes.
  se0 <- mu0 * sqrt(if (q0 > 0) f$raw * q0 else 0)
  tau <- f$phi + se0 * se0 / mu0
  se <- sqrt(4 / 9 * mu0^(1 / 3) * tau)
  z <- .s03qnorm(1 - a / 2)
  U <- (mu0^(2 / 3) + z * se)^1.5
  y0 <- y[t0 + 1L]
  score <- if (U > mu0) (y0 - mu0) / (U - mu0) else Inf
  .t1_result(estimate = score, observed = y0, expected = mu0, threshold = U,
             alarm = if (y0 > U) 1 else 0, phi = f$phi, phi_raw = f$raw,
             trend_coef = if (trend) f$b[2] else 0, score = score,
             nbaseline = nb, n = n,
             method = "Farrington flexible algorithm")
}
