# Methods resolved in the "unclear-attribution" batch: rows whose source
# the surname regex and the triage pass both failed to read.
#
# Mirrors morie.fn._unclrcore (Python) function for function.  Base R
# only, fixed iteration counts, no tolerance-driven early exit.
#
# These are internal parity mirrors, reached as morie:::Name, exactly as
# the rest of the book-as-spec mirrors are: they exist to check the R arm
# against the Python arm, are not part of the exported API, so they add no
# NAMESPACE entries and need no man/*.Rd.
#
# Sources are named per function.  Where a routine is a textbook-standard
# quantity with no single owning source it says so and names none.

# --- small linear algebra (standard; no owning source) ---------------

#' morie_unclr_dft_amp
#'
#' A step of the unclr implementation. Called by \code{Fftperiod}.
#' See the file header for the source the module follows.
#' it follows.
#'
#' @param x Coerced to numeric by the body, with \code{as.numeric}.
#' @return The value of \code{Mod}.
#' @export
#' @examples
#' V <- c(1, 2, 3, 4, 5, 6, 7, 8)
#' morie_unclr_dft_amp(V)
#' @keywords internal
morie_unclr_dft_amp <- function(x) Mod(stats::fft(as.numeric(x)))

#' morie_unclr_phi
#'
#' A step of the unclr implementation. Called by \code{morie_unclr_lr_impute}.
#' See the file header for the source the module follows.
#' it follows.
#'
#' @param z Numeric; combined arithmetically in the body.
#' @return A numeric value.
#' @export
#' @rdname morie_unclr_gaussian
#' @examples
#' y <- c(2.9, 5.1, 6.8, 9.4, 11.2, 13.1, 15.0, 17.6)
#' res <- morie_unclr_phi(z = y)
#' res
morie_unclr_phi <- function(z) exp(-0.5 * z^2) / sqrt(2 * pi)

#' morie_unclr_Phi
#'
#' A step of the unclr implementation. Called by \code{morie_unclr_lr_impute}.
#' See the file header for the source the module follows.
#' it follows.
#'
#' @param z See Usage.
#' @return The value of \code{stats::pnorm}.
#' @export
#' @rdname morie_unclr_gaussian
morie_unclr_Phi <- function(z) stats::pnorm(z)

# ====================================================================
# Lawson, A. B. (2021), Using R for Bayesian Spatial and Spatio-Temporal
# Health Modeling, CRC Press.
# ====================================================================

#' Joint likelihood of independent observations (Lawson eq. 3.1)
#' @param dens Argument `dens`; see Usage.
#' @return A list with `likelihood`, `loglik`, `n`.
#' @examples
#' V <- c(1, 2, 3, 4, 5, 6, 7, 8)
#' rmorie:::Likprod(V)
#' @keywords internal
Likprod <- function(dens) {
  d <- as.numeric(dens)
  if (any(d < 0)) stop("densities must be non-negative")
  list(likelihood = prod(d),
       loglik = if (all(d > 0)) sum(log(d)) else -Inf,
       n = length(d))
}

#' Log-likelihood of independent observations (Lawson eq. 3.2)
#' @param dens Argument `dens`; see Usage.
#' @return A list with `loglik`, `terms`, `n`.
#' @examples
#' V <- c(1, 2, 3, 4, 5, 6, 7, 8)
#' rmorie:::Loglksum(V)
#' @keywords internal
Loglksum <- function(dens) {
  d <- as.numeric(dens)
  if (any(d <= 0)) stop("densities must be strictly positive to take logs")
  list(loglik = sum(log(d)), terms = log(d), n = length(d))
}

#' Posterior-averaged Poisson residual (Lawson eq. 5.2)
#' @param y Argument `y`; see Usage.
#' @param e Argument `e`; see Usage.
#' @param theta_draws Argument `theta_draws`; see Usage.
#' @return A list with `residual`, `fitted`, `n`, `n_draws`.
#' @examples
#' set.seed(1)
#' e <- runif(20, 5, 15)
#' theta <- matrix(rgamma(80, 2, 2), 4, 20)
#' y <- rpois(20, e * colMeans(theta))
#' rmorie:::Postres(y, e, theta)
#' @keywords internal
Postres <- function(y, e, theta_draws) {
  y <- as.numeric(y)
  e <- as.numeric(e)
  Tm <- as.matrix(theta_draws)
  G <- nrow(Tm)
  if (G == 0L) stop("need at least one posterior draw")
  if (length(e) != length(y) || ncol(Tm) != length(y))
    stop("y, e and each posterior draw must have the same length")
  fitted <- as.numeric(e * colMeans(Tm))
  list(residual = y - fitted, fitted = fitted, n = length(y), n_draws = G)
}

#' Modulated point-process intensity (Lawson eq. 6.3)
#' @param lam0 Argument `lam0`; see Usage.
#' @param lam1 Argument `lam1`; see Usage.
#' @return A list with `intensity`, `total`, `n`.
#' @examples
#' V <- c(1, 2, 3, 4, 5, 6, 7, 8)
#' rmorie:::Intmod(V, V)
#' @keywords internal
Intmod <- function(lam0, lam1) {
  a <- as.numeric(lam0)
  b <- as.numeric(lam1)
  if (length(a) != length(b)) stop("lam0 and lam1 must have the same length")
  if (any(a < 0) || any(b < 0)) stop("intensities must be non-negative")
  lam <- a * b
  list(intensity = lam, total = sum(lam), n = length(lam))
}

#' Case-control logistic likelihood (Lawson eq. 6.6)
#' @param eta Argument `eta`; see Usage.
#' @param y Argument `y`; see Usage.
#' @return A list with `loglik`, `p`, `n_cases`, `n`.
#' @examples
#' set.seed(4)
#' rmorie:::Cclogl(eta = rnorm(8), y = rbinom(8, 1, 0.5))
#' @keywords internal
Cclogl <- function(eta, y) {
  e <- as.numeric(eta)
  yy <- as.numeric(y)
  if (length(e) != length(yy)) stop("eta and y must have the same length")
  if (any(!(yy %in% c(0, 1)))) stop("y must be 0/1 (control/case)")
  list(loglik = sum(yy * e - log1p(exp(e))),
       p = 1 / (1 + exp(-e)),
       n_cases = as.integer(sum(yy)), n = length(e))
}

#' Contextual multilevel logit predictor (Lawson eq. 6.8)
#' @param f Argument `f`; see Usage.
#' @param g Argument `g`; see Usage.
#' @param R Argument `R`; see Usage.
#' @return A list with `eta`, `p`, `n`.
#' @examples
#' rmorie:::Mlogitlp(f = c(1, 2, 3, 4, 5, 6, 7, 8), g = c(1, 2, 3, 4, 5, 6, 7, 8),
#'   R = c(1, 2, 3, 4, 5, 6, 7, 8))
#' @keywords internal
Mlogitlp <- function(f, g, R) {
  f <- as.numeric(f)
  g <- as.numeric(g)
  R <- as.numeric(R)
  if (!(length(f) == length(g) && length(g) == length(R)))
    stop("f, g and R must have the same length")
  eta <- f + g + R
  list(eta = eta, p = 1 / (1 + exp(-eta)), n = length(eta))
}

#' Log-Gaussian Cox process intensity (Lawson eq. 6.18)
#' @param lam0 Argument `lam0`; see Usage.
#' @param beta Argument `beta`; see Usage.
#' @param S Argument `S`; see Usage.
#' @return A list with `intensity`, `total`, `beta`, `n`.
#' @examples
#' rmorie:::Lgcpint(lam0 = c(1, 2, 3, 4, 5, 6, 7, 8), beta = 0.5, S = c(1, 2, 3, 4, 5, 6, 7, 8))
#' @keywords internal
Lgcpint <- function(lam0, beta, S) {
  a <- as.numeric(lam0)
  s <- as.numeric(S)
  if (length(a) != length(s)) stop("lam0 and S must have the same length")
  lam <- a * exp(as.numeric(beta) + s)
  list(intensity = lam, total = sum(lam), beta = as.numeric(beta), n = length(lam))
}

#' Spatial factor Poisson log-risk (Lawson eq. 11.1)
#' @param alpha0 Argument `alpha0`; see Usage.
#' @param W Argument `W`; see Usage.
#' @param phi Argument `phi`; see Usage.
#' @return A list with `logrisk`, `risk`, `n`, `n_components`.
#' @examples
#' rmorie:::Facrisk(alpha0 = c(1, 2, 3, 4, 5, 6, 7, 8), W = c(1, 2, 3, 4, 5, 6, 7, 8), phi = 0.5)
#' @keywords internal
Facrisk <- function(alpha0, W, phi) {
  W <- as.matrix(W)
  p <- as.numeric(phi)
  if (ncol(W) != length(p)) stop("each row of W must have one weight per component")
  lr <- as.numeric(alpha0) + as.numeric(W %*% p)
  list(logrisk = lr, risk = exp(lr), n = length(lr), n_components = length(p))
}

#' Shared-factor multivariate disease mean (Lawson eq. 14.1)
#' @param e Argument `e`; see Usage.
#' @param lam Argument `lam`; see Usage.
#' @param f Argument `f`; see Usage.
#' @return A list with `rho`, `mu`, `n`, `n_disease`.
#' @examples
#' rmorie:::Mvfacmu(e = c(1, 2, 3, 4, 5, 6, 7, 8), lam = 5L, f = c(1, 2, 3, 4, 5, 6, 7, 8))
#' @keywords internal
Mvfacmu <- function(e, lam, f) {
  E <- as.matrix(e)
  lv <- as.numeric(lam)
  fv <- as.numeric(f)
  if (nrow(E) != length(fv) || ncol(E) != length(lv))
    stop("e must be n areas by k diseases, matching f and lam")
  rho <- exp(outer(fv, lv))
  list(rho = rho, mu = E * rho, n = length(fv), n_disease = length(lv))
}

#' Multilevel Poisson log-rate (Lawson eq. 15.2)
#' @param beta0 Argument `beta0`; see Usage.
#' @param beta1 Argument `beta1`; see Usage.
#' @param age Argument `age`; see Usage.
#' @param race_effect Argument `race_effect`; see Usage.
#' @param v Argument `v`; see Usage.
#' @param W Argument `W`; see Usage.
#' @return A list with `lograte`, `rate`, `n`.
#' @examples
#' rmorie:::Mlpois(beta0 = c(1, 2, 3, 4, 5, 6, 7, 8), beta1 = c(1, 2, 3, 4, 5, 6, 7, 8),
#'   age = c(1, 2, 3, 4, 5, 6, 7, 8), race_effect = c(1, 2, 3, 4, 5, 6, 7, 8),
#'   v = c(1, 2, 3, 4, 5, 6, 7, 8), W = c(1, 2, 3, 4, 5, 6, 7, 8))
#' @keywords internal
Mlpois <- function(beta0, beta1, age, race_effect, v, W) {
  a <- as.numeric(age)
  r <- as.numeric(race_effect)
  vv <- as.numeric(v)
  ww <- as.numeric(W)
  if (!(length(a) == length(r) && length(r) == length(vv) && length(vv) == length(ww)))
    stop("age, race_effect, v and W must have the same length")
  lr <- as.numeric(beta0) + as.numeric(beta1) * a + r + vv + ww
  list(lograte = lr, rate = exp(lr), n = length(lr))
}

#' Measurement-error normal outcome model (Lawson eq. 16.1)
#' @param beta0 Argument `beta0`; see Usage.
#' @param beta1 Argument `beta1`; see Usage.
#' @param x_true Argument `x_true`; see Usage.
#' @param tau Argument `tau`; see Usage.
#' @return A list with `mu`, `var`, `sd`, `n`.
#' @examples
#' rmorie:::Menorm(beta0 = c(1, 2, 3, 4, 5, 6, 7, 8), beta1 = c(1, 2, 3, 4, 5, 6, 7, 8),
#'   x_true = c(1, 2, 3, 4, 5, 6, 7, 8), tau = 0.5)
#' @keywords internal
Menorm <- function(beta0, beta1, x_true, tau) {
  xt <- as.numeric(x_true)
  t <- as.numeric(tau)
  if (t <= 0) stop("tau is a precision and must be positive")
  mu <- as.numeric(beta0) + as.numeric(beta1) * xt
  list(mu = mu, var = 1 / t, sd = 1 / sqrt(t), n = length(mu))
}

#' Binary spatial regression with random effect (Lawson eq. 17.1)
#' @param gamma0 Argument `gamma0`; see Usage.
#' @param gamma1 Argument `gamma1`; see Usage.
#' @param d Argument `d`; see Usage.
#' @param gamma2 Argument `gamma2`; see Usage.
#' @param x Argument `x`; see Usage.
#' @param R Argument `R`; see Usage.
#' @return A list with `eta`, `p`, `n`.
#' @examples
#' V <- c(1, 2, 3, 4, 5, 6, 7, 8)
#' morie_unclr_dft_amp(V)
#' rmorie:::Logitre(gamma0 = V, gamma1 = V, d = V, gamma2 = V, x = V, R = V)
#' @keywords internal
Logitre <- function(gamma0, gamma1, d, gamma2, x, R) {
  d <- as.numeric(d)
  x <- as.numeric(x)
  R <- as.numeric(R)
  if (!(length(d) == length(x) && length(x) == length(R)))
    stop("d, x and R must have the same length")
  eta <- as.numeric(gamma0) + as.numeric(gamma1) * d + as.numeric(gamma2) * x + R
  list(eta = eta, p = 1 / (1 + exp(-eta)), n = length(eta))
}

#' Epidemic log-autoregression (Lawson eq. 18.3)
#' @param beta0 Argument `beta0`; see Usage.
#' @param beta1 Argument `beta1`; see Usage.
#' @param i_lag Argument `i_lag`; see Usage.
#' @param b1 Argument `b1`; see Usage.
#' @return A list with `logf`, `f`, `n`.
#' @examples
#' rmorie:::Epiar(beta0 = c(1, 2, 3, 4, 5, 6, 7, 8), beta1 = c(1, 2, 3, 4, 5, 6, 7, 8),
#'   i_lag = c(1, 2, 3, 4, 5, 6, 7, 8), b1 = c(1, 2, 3, 4, 5, 6, 7, 8))
#' @keywords internal
Epiar <- function(beta0, beta1, i_lag, b1) {
  il <- as.numeric(i_lag)
  bb <- as.numeric(b1)
  if (length(il) != length(bb)) stop("i_lag and b1 must have the same length")
  if (any(il <= 0)) stop("lagged infective counts must be positive to take logs")
  lf <- as.numeric(beta0) + as.numeric(beta1) * log(il) + bb
  list(logf = lf, f = exp(lf), n = length(lf))
}

#' Epidemic log-autoregression with neighbours (Lawson eq. 18.4)
#' @param beta0 Argument `beta0`; see Usage.
#' @param beta1 Argument `beta1`; see Usage.
#' @param i_lag Argument `i_lag`; see Usage.
#' @param nb_lag Argument `nb_lag`; see Usage.
#' @param b1 Argument `b1`; see Usage.
#' @return A list with `logf`, `f`, `total_lag`, `n`.
#' @examples
#' rmorie:::Epiarnb(beta0 = c(1, 2, 3, 4, 5, 6, 7, 8), beta1 = c(1, 2, 3, 4, 5, 6, 7, 8),
#'   i_lag = c(1, 2, 3, 4, 5, 6, 7, 8), nb_lag = c(1, 2, 3, 4, 5, 6, 7, 8),
#'   b1 = c(1, 2, 3, 4, 5, 6, 7, 8))
#' @keywords internal
Epiarnb <- function(beta0, beta1, i_lag, nb_lag, b1) {
  il <- as.numeric(i_lag)
  nb <- as.numeric(nb_lag)
  bb <- as.numeric(b1)
  if (!(length(il) == length(nb) && length(nb) == length(bb)))
    stop("i_lag, nb_lag and b1 must have the same length")
  tot <- il + nb
  if (any(tot <= 0)) stop("own plus neighbour lagged counts must be positive")
  lf <- as.numeric(beta0) + as.numeric(beta1) * log(tot) + bb
  list(logf = lf, f = exp(lf), total_lag = tot, n = length(lf))
}

# ====================================================================
# Deshmukh, S. R. & Kashikar, A. S. (2021), Probability Theory: An
# Introduction Using R, CRC Press.
# ====================================================================

#' Characteristic-function inversion for a pmf (Deshmukh eq. 4.9)
#' @param t Argument `t`; see Usage.
#' @param phi_re Argument `phi_re`; see Usage.
#' @param phi_im Argument `phi_im`; see Usage.
#' @param x Argument `x`; see Usage.
#' @return A list with `pmf`, `x`, `n_nodes`.
#' @examples
#' rmorie:::Cfinvpmf(t = c(1, 2, 3, 4, 5, 6, 7, 8), phi_re = c(1, 2, 3, 4, 5, 6, 7, 8),
#'   phi_im = c(1, 2, 3, 4, 5, 6, 7, 8), x = c(1, 2, 3, 4, 5, 6, 7, 8))
#' @keywords internal
Cfinvpmf <- function(t, phi_re, phi_im, x) {
  tv <- as.numeric(t)
  pr <- as.numeric(phi_re)
  pim <- as.numeric(phi_im)
  if (!(length(tv) == length(pr) && length(pr) == length(pim)))
    stop("t, phi_re and phi_im must have the same length")
  if (length(tv) < 2L) stop("need at least two quadrature nodes")
  xv <- as.numeric(x)
  out <- numeric(length(xv))
  nn <- length(tv)
  for (q in seq_along(xv)) {
    g <- cos(tv * xv[q]) * pr + sin(tv * xv[q]) * pim
    out[q] <- sum((g[-nn] + g[-1]) * diff(tv) / 2) / (2 * pi)
  }
  list(pmf = out, x = xv, n_nodes = nn)
}

#' Independence of k events (Deshmukh eq. 5.1)
#' @param p Argument `p`; see Usage.
#' @param joint Argument `joint`; see Usage.
#' @return A list with `n_conditions`, `max_deviation`, `independent`, `k`.
#' @examples
#' V <- c(1, 2, 3, 4, 5, 6, 7, 8)
#' S <- c("a", "b", "c")
#' rmorie:::Indevk(S, V)
#' @keywords internal
Indevk <- function(p, joint) {
  pv <- as.numeric(p)
  k <- length(pv)
  jv <- as.numeric(joint)
  if (length(jv) != 2^k)
    stop(sprintf("joint must hold one probability per subset mask, i.e. %d entries", 2^k))
  worst <- 0
  for (m in seq_len(2^k) - 1L) {
    bits <- which(bitwAnd(m, 2^(seq_len(k) - 1L)) > 0)
    if (length(bits) < 2L) next
    worst <- max(worst, abs(jv[m + 1L] - prod(pv[bits])))
  }
  list(n_conditions = 2^k - k - 1, max_deviation = worst,
       independent = worst <= 1e-12, k = k)
}

#' Independence of two random variables (Deshmukh eq. 5.3)
#' @param joint Argument `joint`; see Usage.
#' @return A list with `max_deviation`, `independent`, `margin_row`, `margin_col`.
#' @examples
#' J <- outer(c(0.3, 0.7), c(0.4, 0.6))
#' rmorie:::Indrv2(J)
#' @keywords internal
Indrv2 <- function(joint) {
  J <- as.matrix(joint)
  tot <- sum(J)
  if (abs(tot - 1) > 1e-9) stop(sprintf("joint probabilities must sum to 1, got %s", tot))
  rows <- rowSums(J)
  cols <- colSums(J)
  worst <- max(abs(J - outer(rows, cols)))
  list(max_deviation = worst, independent = worst <= 1e-12,
       margin_row = rows, margin_col = cols)
}

#' Limit-superior event, infinitely often (Deshmukh eq. 6.1)
#' @param dev Argument `dev`; see Usage.
#' @param k Argument `k`; see Usage.
#' @return A list with `threshold`, `in_event`, `prob`, `n_paths`.
#' @examples
#' rmorie:::Limsupio(dev = c(1, 2, 3, 4, 5, 6, 7, 8), k = 5L)
#' @keywords internal
Limsupio <- function(dev, k) {
  D <- as.matrix(dev)
  kk <- as.integer(k)
  if (kk < 1L) stop("k must be a positive integer")
  thr <- 1 / kk
  flags <- as.integer(D[, ncol(D)] >= thr)
  list(threshold = thr, in_event = flags, prob = sum(flags) / length(flags),
       n_paths = nrow(D))
}

#' Degenerate limiting distribution of the sample mean (Deshmukh eq. 10.3)
#' @param x Argument `x`; see Usage.
#' @param mu Argument `mu`; see Usage.
#' @return A list with `cdf`, `mu`, `x`.
#' @examples
#' V <- c(1, 2, 3, 4, 5, 6, 7, 8)
#' rmorie:::Degencdf(V, V)
#' @keywords internal
Degencdf <- function(x, mu) {
  xv <- as.numeric(x)
  m <- as.numeric(mu)
  out <- ifelse(xv < m, 0, ifelse(xv > m, 1, NaN))
  list(cdf = out, mu = m, x = xv)
}

# ====================================================================
# Klein, D. J. & Randic, M. (1993), Resistance distance, Journal of
# Mathematical Chemistry 12, 81-95.
# ====================================================================

#' Graph Laplacian pseudoinverse (Klein & Randic 1993)
#' @param A Argument `A`; see Usage.
#' @param tol Argument `tol`; see Usage.
#' @return A list with `Lplus`, `eigenvalues`, `rank`, `n`.
#' @examples
#' A <- rbind(c(0, 1, 1, 0), c(1, 0, 1, 0), c(1, 1, 0, 1), c(0, 0, 1, 0))
#' rmorie:::Lappinv(A)
#' @keywords internal
Lappinv <- function(A, tol = 1e-9) {
  A <- as.matrix(A)
  n <- nrow(A)
  if (ncol(A) != n) stop("A must be square")
  if (max(abs(A - t(A))) > 1e-12) stop("A must be symmetric")
  L <- diag(rowSums(A), n) - A
  ev <- eigen(L, symmetric = TRUE)
  keep <- abs(ev$values) > tol
  Lp <- matrix(0, n, n)
  if (any(keep)) {
    V <- ev$vectors[, keep, drop = FALSE]
    Lp <- V %*% diag(1 / ev$values[keep], sum(keep)) %*% t(V)
  }
  list(Lplus = Lp, eigenvalues = ev$values, rank = sum(keep), n = n)
}

#' Resistance distance matrix (Klein & Randic 1993)
#' @param A Argument `A`; see Usage.
#' @param tol Argument `tol`; see Usage.
#' @return A list with `R`, `Lplus`, `n`, `rank`.
#' @examples
#' A <- rbind(c(0, 1, 1, 0), c(1, 0, 1, 0), c(1, 1, 0, 1), c(0, 0, 1, 0))
#' rmorie:::Resdist(A)
#' @keywords internal
Resdist <- function(A, tol = 1e-9) {
  lp <- Lappinv(A, tol)
  Lp <- lp$Lplus
  n <- lp$n
  d <- diag(Lp)
  R <- outer(d, d, "+") - 2 * Lp
  list(R = R, Lplus = Lp, n = n, rank = lp$rank)
}

#' Commute-time distance (Klein & Randic 1993; Chandra et al. 1989)
#' @param A Argument `A`; see Usage.
#' @param tol Argument `tol`; see Usage.
#' @return A list with `C`, `R`, `two_m`, `n`.
#' @examples
#' A <- rbind(c(0, 1, 1, 0), c(1, 0, 1, 0), c(1, 1, 0, 1), c(0, 0, 1, 0))
#' rmorie:::Commdist(A)
#' @keywords internal
Commdist <- function(A, tol = 1e-9) {
  rd <- Resdist(A, tol)
  m2 <- sum(as.matrix(A))
  list(C = m2 * rd$R, R = rd$R, two_m = m2, n = rd$n)
}

#' Kirchhoff index (Klein & Randic 1993)
#' @param A Argument `A`; see Usage.
#' @param tol Argument `tol`; see Usage.
#' @return A list with `Kf`, `Kf_spectral`, `n`, `rank`.
#' @examples
#' A <- rbind(c(0, 1, 1, 0), c(1, 0, 1, 0), c(1, 1, 0, 1), c(0, 0, 1, 0))
#' rmorie:::Kirchidx(A)
#' @keywords internal
Kirchidx <- function(A, tol = 1e-9) {
  rd <- Resdist(A, tol)
  n <- rd$n
  lp <- Lappinv(A, tol)
  nz <- abs(lp$eigenvalues) > tol
  list(Kf = 0.5 * sum(rd$R), Kf_spectral = n * sum(1 / lp$eigenvalues[nz]),
       n = n, rank = lp$rank)
}

# ====================================================================
# Memoli, F. (2011), Gromov-Wasserstein distances, Found. Comput. Math.
# 11, 417-487.  Solver: Peyre, Cuturi & Solomon (2016), ICML.
# ====================================================================

#' Gromov-Wasserstein discrepancy (Memoli 2011)
#' @param Cx Argument `Cx`; see Usage.
#' @param Cy Argument `Cy`; see Usage.
#' @param a Argument `a`; see Usage.
#' @param b Argument `b`; see Usage.
#' @param n_iter Argument `n_iter`; see Usage.
#' @param epsilon Argument `epsilon`; see Usage.
#' @param n_sinkhorn Argument `n_sinkhorn`; see Usage.
#' @return A list with `T`, `cost`, `cost_product`, `n_iter`, `n`, `m`.
#' @examples
#' set.seed(5)
#' Cx <- as.matrix(dist(matrix(rnorm(10), 5, 2)))
#' Cy <- as.matrix(dist(matrix(rnorm(10), 5, 2)))
#' rmorie:::Gwdist(Cx, Cy, a = rep(0.2, 5), b = rep(0.2, 5), n_iter = 10)
#' @keywords internal
Gwdist <- function(Cx, Cy, a, b, n_iter = 50, epsilon = 0.05, n_sinkhorn = 50) {
  X <- as.matrix(Cx)
  Y <- as.matrix(Cy)
  av <- as.numeric(a)
  bv <- as.numeric(b)
  n <- length(av)
  m <- length(bv)
  if (nrow(X) != n || nrow(Y) != m)
    stop("Cx must be n x n and Cy must be m x m, matching a and b")
  if (abs(sum(av) - 1) > 1e-9 || abs(sum(bv) - 1) > 1e-9)
    stop("a and b must each sum to 1")
  cost <- function(Tm) {
    tot <- 0
    for (i in seq_len(n)) for (k in seq_len(m)) {
      if (Tm[i, k] == 0) next
      for (j in seq_len(n)) {
        e <- X[i, j] - Y[k, ]
        tot <- tot + sum(e * e * Tm[i, k] * Tm[j, ])
      }
    }
    tot
  }
  grad <- function(Tm) {
    G <- matrix(0, n, m)
    for (i in seq_len(n)) for (k in seq_len(m)) {
      s <- 0
      for (j in seq_len(n)) {
        e <- X[i, j] - Y[k, ]
        s <- s + sum(e * e * Tm[j, ])
      }
      G[i, k] <- 2 * s
    }
    G
  }
  Tm <- outer(av, bv)
  c0 <- cost(Tm)
  for (it in seq_len(as.integer(n_iter))) {
    G <- grad(Tm)
    K <- exp(-G / epsilon) * Tm
    u <- rep(1, n)
    v <- rep(1, m)
    for (s in seq_len(as.integer(n_sinkhorn))) {
      den <- as.numeric(K %*% v)
      u <- ifelse(den > 0, av / den, 0)
      den2 <- as.numeric(t(K) %*% u)
      v <- ifelse(den2 > 0, bv / den2, 0)
    }
    Tm <- (u %o% v) * K
  }
  list(T = Tm, cost = cost(Tm), cost_product = c0,
       n_iter = as.integer(n_iter), n = n, m = m)
}

# ====================================================================
# Palarea-Albaladejo & Martin-Fernandez, lrEM (2008) / lrDA (2013).
# ====================================================================

#' morie_unclr_alr
#'
#' A step of the unclr implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' it follows.
#'
#' @param x A vector; its length is taken and its elements indexed.
#' @return A numeric value.
#' @export
#' @examples
#' V <- c(1, 2, 3, 4, 5, 6, 7, 8)
#' morie_unclr_alr(V)
#' @keywords internal
morie_unclr_alr <- function(x) log(x[-length(x)] / x[length(x)])

#' morie_unclr_alr_inv
#'
#' A step of the unclr implementation. Called by \code{morie_unclr_lr_impute}.
#' See the file header for the source the module follows.
#' it follows.
#'
#' @param z Numeric; passed to \code{exp}.
#' @param total Numeric; combined arithmetically in the body.
#' @return A numeric value.
#' @export
#' @examples
#' V <- c(1, 2, 3, 4, 5, 6, 7, 8)
#' morie_unclr_alr_inv(V, V)
#' @keywords internal
morie_unclr_alr_inv <- function(z, total) {
  e <- c(exp(z), 1)
  total * e / sum(e)
}

#' morie_unclr_lr_impute
#'
#' A step of the unclr implementation. Called by \code{Lrda}, \code{Lrem}.
#' See the file header for the source the module follows.
#' it follows.
#'
#' @param X A matrix; passed to \code{as.matrix}.
#' @param dl Coerced to numeric by the body, with \code{as.numeric}.
#' @param n_iter Coerced to integer by the body, with \code{as.integer}.
#' @param draw Optional; may be \code{NULL}. A matrix; passed to \code{as.matrix}.
#' @return A list with \code{X}, \code{n}, \code{n_parts}, \code{n_iter}, \code{n_censored}.
#' @export
#' @examples
#' morie_unclr_lr_impute(X = c(1, 2, 3, 4, 5, 6, 7, 8), dl = 5L, n_iter = c(1, 2, 3, 4, 5, 6, 7, 8))
#' @keywords internal
morie_unclr_lr_impute <- function(X, dl, n_iter, draw = NULL) {
  Xm <- as.matrix(X)
  n <- nrow(Xm)
  d <- ncol(Xm)
  dlv <- as.numeric(dl)
  if (length(dlv) != d) stop("dl must give one detection limit per part")
  totals <- rowSums(Xm)
  cens <- sweep(Xm, 2, dlv, "<")
  W <- Xm
  for (j in seq_len(d)) W[cens[, j], j] <- 0.65 * dlv[j]
  p <- d - 1L
  for (t in seq_len(as.integer(n_iter))) {
    Z <- t(apply(W, 1, morie_unclr_alr))
    if (p == 1L) Z <- matrix(Z, ncol = 1L)
    mu <- colMeans(Z)
    S <- stats::var(Z)
    if (p == 1L) S <- matrix(S, 1, 1)
    for (i in seq_len(n)) {
      for (j in seq_len(p)) {
        if (!cens[i, j]) next
        obs <- setdiff(seq_len(p), j)
        psi <- log(dlv[j] / W[i, d])
        if (length(obs) > 0L) {
          Soo <- S[obs, obs, drop = FALSE]
          sjo <- S[j, obs]
          w <- solve(Soo + diag(1e-10, length(obs)), sjo)
          cm <- mu[j] + sum(w * (Z[i, obs] - mu[obs]))
          cv <- S[j, j] - sum(w * sjo)
        } else {
          cm <- mu[j]
          cv <- S[j, j]
        }
        sdv <- sqrt(max(cv, 1e-300))
        alpha <- (psi - cm) / sdv
        if (is.null(draw)) {
          den <- morie_unclr_Phi(alpha)
          zj <- if (den > 1e-300) cm - sdv * (morie_unclr_phi(alpha) / den) else psi
        } else {
          Dm <- as.matrix(draw)
          u <- Dm[(t - 1L) %% nrow(Dm) + 1L, (i - 1L) %% ncol(Dm) + 1L]
          zj <- min(cm + sdv * u, psi)
        }
        Z[i, j] <- zj
        W[i, ] <- morie_unclr_alr_inv(Z[i, ], totals[i])
      }
    }
  }
  list(X = W, n = n, n_parts = d, n_iter = as.integer(n_iter),
       n_censored = sum(cens))
}

#' Log-ratio EM below a detection limit (Palarea-Albaladejo & Martin-Fernandez 2008)
#' @param X Argument `X`; see Usage.
#' @param dl Argument `dl`; see Usage.
#' @param n_iter Argument `n_iter`; see Usage.
#' @return The value of `morie_unclr_lr_impute`.
#' @examples
#' rmorie:::Lrem(X = c(1, 2, 3, 4, 5, 6, 7, 8), dl = 5L)
#' @keywords internal
Lrem <- function(X, dl, n_iter = 20) morie_unclr_lr_impute(X, dl, n_iter, NULL)

#' Log-ratio data augmentation below a detection limit (Palarea-Albaladejo et al. 2013)
#' @param X Argument `X`; see Usage.
#' @param dl Argument `dl`; see Usage.
#' @param draw Argument `draw`; see Usage.
#' @param n_iter Argument `n_iter`; see Usage.
#' @return The value of `morie_unclr_lr_impute`.
#' @examples
#' rmorie:::Lrda(X = c(1, 2, 3, 4, 5, 6, 7, 8), dl = 5L, draw = c(1, 2, 3, 4, 5, 6, 7, 8))
#' @keywords internal
Lrda <- function(X, dl, draw, n_iter = 20) {
  if (is.null(draw)) stop("lrda needs caller-supplied standard normal variates")
  morie_unclr_lr_impute(X, dl, n_iter, draw)
}

# ====================================================================
# Shao, J. & Wu, C. F. J. (1989), Annals of Statistics 17, 1176-1197.
# ====================================================================

#' Delete-d jackknife variance (Shao & Wu 1989)
#' @param theta Argument `theta`; see Usage.
#' @param n Argument `n`; see Usage.
#' @param d Argument `d`; see Usage.
#' @return A list with `variance`, `se`, `mean`, `n_subsets`, `n`, `d`.
#' @examples
#' set.seed(4)
#' rmorie:::Jackd(theta = rnorm(10, 2, 0.1), n = 10, d = 1)
#' @keywords internal
Jackd <- function(theta, n, d) {
  tv <- as.numeric(theta)
  n <- as.integer(n)
  d <- as.integer(d)
  if (d < 1L || d >= n) stop("d must satisfy 1 <= d < n")
  nsub <- choose(n, d)
  if (length(tv) != nsub)
    stop(sprintf("expected one estimate per size-(n-d) subset, i.e. %d, got %d", nsub, length(tv)))
  bar <- mean(tv)
  v <- (n - d) / (d * nsub) * sum((tv - bar)^2)
  list(variance = v, se = sqrt(max(v, 0)), mean = bar,
       n_subsets = nsub, n = n, d = d)
}

# ====================================================================
# Gibbons & Chakraborti, Nonparametric Statistical Inference,
# Theorems 7.3.1-7.3.2.
# ====================================================================

#' Moments of a linear rank statistic (Gibbons & Chakraborti Thm 7.3.1-7.3.2)
#' @param a Argument `a`; see Usage.
#' @param m Argument `m`; see Usage.
#' @return A list with `mean`, `variance`, `se`, `score_mean`, `N`, `m`, `n`.
#' @examples
#' rmorie:::Lrankmom(a = c(1, 2, 3, 4, 5, 6, 7, 8), m = 5L)
#' @keywords internal
Lrankmom <- function(a, m) {
  av <- as.numeric(a)
  N <- length(av)
  m <- as.integer(m)
  if (!(m > 0L && m < N)) stop("m must satisfy 0 < m < N")
  n <- N - m
  abar <- mean(av)
  ss <- sum((av - abar)^2)
  v <- m * n * ss / (N * (N - 1))
  list(mean = m * abar, variance = v, se = sqrt(max(v, 0)),
       score_mean = abar, N = N, m = m, n = n)
}

# ====================================================================
# Cole, S. R. & Hernan, M. A. (2008), Am. J. Epidemiol. 168, 656-664.
# ====================================================================

#' Inverse-probability weight truncation (Cole & Hernan 2008)
#' @param w Argument `w`; see Usage.
#' @param q Argument `q`; see Usage.
#' @return A list with `weights`, `cap`, `n_truncated`, `n`, `mean_before`, `mean_after`.
#' @examples
#' V <- c(1, 2, 3, 4, 5, 6, 7, 8)
#' rmorie:::Wtrunc(V)
#' @keywords internal
Wtrunc <- function(w, q = 0.99) {
  wv <- as.numeric(w)
  if (any(wv < 0)) stop("weights must be non-negative")
  if (!(q > 0 && q <= 1)) stop("q must lie in (0, 1]")
  cap <- as.numeric(stats::quantile(wv, probs = q, type = 7, names = FALSE))
  out <- pmin(wv, cap)
  list(weights = out, cap = cap, n_truncated = sum(wv > cap),
       n = length(wv), mean_before = mean(wv), mean_after = mean(out))
}

# ====================================================================
# Yu, J. et al. (2006), Nature Genetics 38, 203-208.
# ====================================================================

#' Unified mixed-model per-SNP association test (Yu et al. 2006)
#' @param y Argument `y`; see Usage.
#' @param X Argument `X`; see Usage.
#' @param snp Argument `snp`; see Usage.
#' @param Vinv Argument `Vinv`; see Usage.
#' @return A list with `beta`, `se`, `statistic`, `df`, `coefficients`, `sigma2`, `n`.
#' @examples
#' set.seed(2)
#' n <- 30
#' X <- cbind(1, rnorm(n))
#' snp <- rbinom(n, 2, 0.3)
#' y <- X %*% c(1, 0.5) + 0.4 * snp + rnorm(n, 0, 0.5)
#' rmorie:::Gwasmlm(as.numeric(y), X, snp, diag(n))
#' @keywords internal
Gwasmlm <- function(y, X, snp, Vinv) {
  yv <- as.numeric(y)
  g <- as.numeric(snp)
  Xm <- as.matrix(X)
  Vi <- as.matrix(Vinv)
  n <- length(yv)
  if (length(g) != n || nrow(Xm) != n || nrow(Vi) != n)
    stop("y, X, snp and Vinv must all have n rows")
  D <- cbind(Xm, g)
  p <- ncol(D)
  A <- t(D) %*% Vi %*% D
  rhs <- t(D) %*% Vi %*% yv
  beta <- as.numeric(solve(A, rhs))
  resid <- yv - as.numeric(D %*% beta)
  rVr <- as.numeric(t(resid) %*% Vi %*% resid)
  dfres <- n - p
  if (dfres <= 0L) stop("no residual degrees of freedom")
  s2 <- rVr / dfres
  Ainv_last <- solve(A, c(rep(0, p - 1), 1))
  se <- sqrt(max(s2 * Ainv_last[p], 0))
  b <- beta[p]
  list(beta = b, se = se, statistic = if (se > 0) b / se else NaN,
       df = dfres, coefficients = beta, sigma2 = s2, n = n)
}

# ====================================================================
# Li, Meng, Raghunathan & Rubin (1991), Statistica Sinica 1, 65-92.
# ====================================================================

#' Multiply-imputed Wald test (Li et al. 1991)
#' @param theta Argument `theta`; see Usage.
#' @param U Argument `U`; see Usage.
#' @return A list with `statistic`, `df1`, `df2`, `r`, `estimate`, `m`.
#' @examples
#' V <- c(1, 2, 3, 4, 5, 6, 7, 8)
#' rmorie:::Mitest(V, V)
#' @keywords internal
Mitest <- function(theta, U) {
  Th <- as.matrix(theta)
  m <- nrow(Th)
  k <- ncol(Th)
  if (m < 2L) stop("need at least 2 imputations")
  qbar <- colMeans(Th)
  Ubar <- Reduce(`+`, lapply(U, as.matrix)) / m
  cent <- sweep(Th, 2, qbar)
  B <- (t(cent) %*% cent) / (m - 1)
  tr <- sum(diag(solve(Ubar, B)))
  r1 <- (1 + 1 / m) * tr / k
  Tm <- Ubar * (1 + r1)
  D1 <- as.numeric(t(qbar) %*% solve(Tm, qbar)) / k
  a <- k * (m - 1)
  v <- if (a > 4) 4 + (a - 4) * (1 + (1 - 2 / a) / r1)^2
       else 0.5 * a * (1 + 1 / k) * (1 + 1 / r1)^2
  list(statistic = D1, df1 = k, df2 = v, r = r1, estimate = qbar, m = m)
}

# ====================================================================
# Ge, T. et al. (2019), Nature Communications 10, 1776 (PRS-CS).
# ====================================================================

#' Continuous-shrinkage polygenic effects (Ge et al. 2019)
#' @param beta_hat Argument `beta_hat`; see Usage.
#' @param D Argument `D`; see Usage.
#' @param psi Argument `psi`; see Usage.
#' @param n Argument `n`; see Usage.
#' @param sigma2 Argument `sigma2`; see Usage.
#' @return A list with `beta`, `shrinkage`, `n`, `sigma2`, `n_snp`.
#' @examples
#' rmorie:::Csshrink(beta_hat = 0.2, D = 1, psi = 0.25, n = 1000)$beta  # 0.2 / (1 + 4)
#' @keywords internal
Csshrink <- function(beta_hat, D, psi, n, sigma2 = 1) {
  bh <- as.numeric(beta_hat)
  Dm <- as.matrix(D)
  ps <- as.numeric(psi)
  p <- length(bh)
  if (nrow(Dm) != p || length(ps) != p)
    stop("beta_hat, D and psi must be conformable")
  if (any(ps <= 0)) stop("psi entries must be positive")
  nn <- as.numeric(n)
  # Ge et al. (2019): the posterior mean is (D + Psi^-1)^-1 beta_hat;
  # n and sigma2 cancel from it (PRS-CS sampler: ld_blk + diag(1/psi))
  A <- Dm + diag(1 / ps, p)
  post <- as.numeric(solve(A, bh))
  list(beta = post, shrinkage = ifelse(bh != 0, post / bh, NaN),
       n = nn, sigma2 = as.numeric(sigma2), n_snp = p)
}

# ====================================================================
# Piecewise log-linear shedding curve (standard least squares).  He, X.
# et al. (2020), Nature Medicine 26, 672-675 is cited for the three-phase
# shape it reports, not for this fit.
# ====================================================================

#' Piecewise log-linear shedding curve (standard)
#' @param days Argument `days`; see Usage.
#' @param load Argument `load`; see Usage.
#' @param t_peak Argument `t_peak`; see Usage.
#' @param t_plateau Argument `t_plateau`; see Usage.
#' @return A list with `rise_slope`, `rise_intercept`, `plateau_level`, `decay_slope`, `decay_intercept`, `peak_load`, `peak_day`, `n`, `n_rise`, `n_plateau`, `n_decay`.
#' @examples
#' set.seed(2)
#' days <- 0:14
#' load <- 10^(c(seq(2, 6, length.out = 5), seq(5.8, 4, length.out = 5),
#'               rep(3.9, 5)) + rnorm(15, 0, 0.05))
#' rmorie:::Shedcurve(days, load, t_peak = 4, t_plateau = 9)
#' @keywords internal
Shedcurve <- function(days, load, t_peak, t_plateau) {
  d <- as.numeric(days)
  v <- as.numeric(load)
  if (length(d) != length(v)) stop("days and load must have the same length")
  if (any(v <= 0)) stop("viral load must be positive to take log10")
  y <- log10(v)
  tp <- as.numeric(t_peak)
  tq <- as.numeric(t_plateau)
  if (!(tp < tq)) stop("t_peak must be strictly before t_plateau")
  slope <- function(idx) {
    if (length(idx) < 2L)
      return(c(NaN, if (length(idx) > 0L) mean(y[idx]) else NaN))
    xs <- d[idx]
    ys <- y[idx]
    mx <- mean(xs)
    my <- mean(ys)
    sxx <- sum((xs - mx)^2)
    if (sxx == 0) return(c(NaN, my))
    b <- sum((xs - mx) * (ys - my)) / sxx
    c(b, my - b * mx)
  }
  rise <- which(d < tp)
  plat <- which(d >= tp & d <= tq)
  dec <- which(d > tq)
  sr <- slope(rise)
  sd_ <- slope(dec)
  list(rise_slope = sr[1], rise_intercept = sr[2],
       plateau_level = if (length(plat)) mean(y[plat]) else NaN,
       decay_slope = sd_[1], decay_intercept = sd_[2],
       peak_load = max(y), peak_day = d[which.max(y)],
       n = length(d), n_rise = length(rise), n_plateau = length(plat),
       n_decay = length(dec))
}

# ====================================================================
# Zheng, W. & van der Laan, M. J., CV-TMLE (2011) and natural direct
# effects (2012).
# ====================================================================

#' Cross-validated TMLE of the ATE (Zheng & van der Laan 2011)
#' @param y Argument `y`; see Usage.
#' @param a Argument `a`; see Usage.
#' @param q0 Argument `q0`; see Usage.
#' @param q1 Argument `q1`; see Usage.
#' @param g Argument `g`; see Usage.
#' @param fold Argument `fold`; see Usage.
#' @param n_newton Argument `n_newton`; see Usage.
#' @return A list with `estimate`, `se`, `psi_fold`, `eps_fold`, `n_folds`, `n`.
#' @examples
#' set.seed(3)
#' n <- 60
#' a <- rbinom(n, 1, 0.5)
#' g <- rep(0.5, n)
#' q1 <- rep(0.6, n); q0 <- rep(0.4, n)
#' y <- rbinom(n, 1, ifelse(a == 1, 0.6, 0.4))
#' fold <- rep(1:3, length.out = n)
#' rmorie:::Cvtmle(y, a, q0, q1, g, fold)
#' @keywords internal
Cvtmle <- function(y, a, q0, q1, g, fold, n_newton = 50) {
  yv <- as.numeric(y)
  av <- as.numeric(a)
  g0 <- as.numeric(q0)
  g1 <- as.numeric(q1)
  gv <- as.numeric(g)
  fv <- as.integer(fold)
  n <- length(yv)
  if (!all(c(length(av), length(g0), length(g1), length(gv), length(fv)) == n))
    stop("all inputs must have the same length")
  if (any(yv < 0 | yv > 1)) stop("y must be bounded in [0, 1]")
  if (any(gv <= 0 | gv >= 1)) stop("propensities must lie strictly inside (0, 1)")
  lg <- function(p) { p <- pmin(pmax(p, 1e-12), 1 - 1e-12)
  log(p / (1 - p)) }
  folds <- sort(unique(fv))
  psi_fold <- numeric(length(folds))
  eps_fold <- numeric(length(folds))
  for (q in seq_along(folds)) {
    idx <- which(fv == folds[q])
    H <- av[idx] / gv[idx] - (1 - av[idx]) / (1 - gv[idx])
    Qa <- ifelse(av[idx] == 1, g1[idx], g0[idx])
    eps <- 0
    for (s in seq_len(as.integer(n_newton))) {
      p <- 1 / (1 + exp(-(lg(Qa) + eps * H)))
      sc <- sum(H * (yv[idx] - p))
      dsc <- -sum(H * H * p * (1 - p))
      if (dsc == 0) break
      eps <- eps - sc / dsc
    }
    q1s <- 1 / (1 + exp(-(lg(g1[idx]) + eps * (1 / gv[idx]))))
    q0s <- 1 / (1 + exp(-(lg(g0[idx]) - eps * (1 / (1 - gv[idx])))))
    psi_fold[q] <- mean(q1s - q0s)
    eps_fold[q] <- eps
  }
  psi <- mean(psi_fold)
  H <- av / gv - (1 - av) / (1 - gv)
  Qa <- ifelse(av == 1, g1, g0)
  ic <- H * (yv - Qa) + (g1 - g0) - psi
  list(estimate = psi, se = sqrt(sum(ic^2) / n / n),
       psi_fold = psi_fold, eps_fold = eps_fold,
       n_folds = length(folds), n = n)
}

#' Natural direct effect (Zheng & van der Laan 2012)
#' @param y10 Argument `y10`; see Usage.
#' @param y00 Argument `y00`; see Usage.
#' @return A list with `estimate`, `se`, `mean_y10`, `mean_y00`, `n`.
#' @examples
#' V <- c(1, 2, 3, 4, 5, 6, 7, 8)
#' rmorie:::Ndeff(V, V)
#' @keywords internal
Ndeff <- function(y10, y00) {
  a <- as.numeric(y10)
  b <- as.numeric(y00)
  if (length(a) != length(b)) stop("y10 and y00 must have the same length")
  d <- a - b
  n <- length(d)
  list(estimate = mean(d), se = if (n > 1L) sqrt(stats::var(d) / n) else NaN,
       mean_y10 = mean(a), mean_y00 = mean(b), n = n)
}

#' Natural indirect effect (Zheng & van der Laan 2012)
#' @param y11 Argument `y11`; see Usage.
#' @param y10 Argument `y10`; see Usage.
#' @return A list with `estimate`, `se`, `mean_y11`, `mean_y10`, `n`.
#' @examples
#' V <- c(1, 2, 3, 4, 5, 6, 7, 8)
#' rmorie:::Nieff(V, V)
#' @keywords internal
Nieff <- function(y11, y10) {
  a <- as.numeric(y11)
  b <- as.numeric(y10)
  if (length(a) != length(b)) stop("y11 and y10 must have the same length")
  d <- a - b
  n <- length(d)
  list(estimate = mean(d), se = if (n > 1L) sqrt(stats::var(d) / n) else NaN,
       mean_y11 = mean(a), mean_y10 = mean(b), n = n)
}

# ====================================================================
# Kunzel, Sekhon, Bickel & Yu (2019), PNAS 116, 4156-4165.
# ====================================================================

#' X-learner heterogeneous treatment effect (Kunzel et al. 2019)
#' @param tau1 Argument `tau1`; see Usage.
#' @param tau0 Argument `tau0`; see Usage.
#' @param g Argument `g`; see Usage.
#' @return A list with `tau`, `ate`, `se`, `n`.
#' @examples
#' set.seed(3)
#' rmorie:::Xlearn(tau1 = rnorm(20, 1), tau0 = rnorm(20, 0.8), g = runif(20, 0.3, 0.7))
#' @keywords internal
Xlearn <- function(tau1, tau0, g) {
  t1 <- as.numeric(tau1)
  t0 <- as.numeric(tau0)
  gv <- as.numeric(g)
  if (!(length(t1) == length(t0) && length(t0) == length(gv)))
    stop("tau1, tau0 and g must have the same length")
  if (any(gv < 0 | gv > 1)) stop("propensities must lie in [0, 1]")
  tau <- gv * t0 + (1 - gv) * t1
  n <- length(tau)
  list(tau = tau, ate = mean(tau),
       se = if (n > 1L) sqrt(stats::var(tau) / n) else NaN, n = n)
}

# ====================================================================
# Deterministic components of published neural architectures.  Nothing
# here is trained: each is a closed-form operation from its paper,
# evaluated on caller-supplied weights.
# ====================================================================

#' Rotary position embedding (Su et al. 2021, RoFormer)
#' @param q Argument `q`; see Usage.
#' @param m Argument `m`; see Usage.
#' @param theta Argument `theta`; see Usage.
#' @return A list with `q`, `m`, `norm`, `n`.
#' @examples
#' rmorie:::Rope(q = c(1, 0, 0.5, -0.5), m = 3,
#'      theta = 10000^(-c(0, 1) / 2))
#' @keywords internal
Rope <- function(q, m, theta) {
  qv <- as.numeric(q)
  th <- as.numeric(theta)
  if (length(qv) %% 2L != 0L) stop("q must have even length (rotations act on pairs)")
  half <- length(qv) %/% 2L
  if (length(th) != half) stop("theta must give one angle per coordinate pair")
  mm <- as.numeric(m)
  out <- numeric(length(qv))
  for (i in seq_len(half)) {
    cc <- cos(mm * th[i])
    ss <- sin(mm * th[i])
    a <- qv[2 * i - 1]
    b <- qv[2 * i]
    out[2 * i - 1] <- cc * a - ss * b
    out[2 * i] <- ss * a + cc * b
  }
  list(q = out, m = mm, norm = sqrt(sum(out^2)), n = length(qv))
}

#' Group normalisation (Wu & He 2018)
#' @param x Argument `x`; see Usage.
#' @param n_groups Argument `n_groups`; see Usage.
#' @param eps Argument `eps`; see Usage.
#' @return A list with `x`, `mean`, `sd`, `n_groups`, `group_size`.
#' @examples
#' set.seed(4)
#' rmorie:::Grpnorm(rnorm(12), n_groups = 3)
#' @keywords internal
Grpnorm <- function(x, n_groups, eps = 1e-5) {
  xv <- as.numeric(x)
  G <- as.integer(n_groups)
  if (G < 1L || length(xv) %% G != 0L)
    stop("length of x must be divisible by n_groups")
  per <- length(xv) %/% G
  out <- numeric(length(xv))
  mus <- numeric(G)
  sds <- numeric(G)
  for (g in seq_len(G)) {
    ix <- ((g - 1L) * per + 1L):(g * per)
    seg <- xv[ix]
    mu <- mean(seg)
    sdv <- sqrt(sum((seg - mu)^2) / per + as.numeric(eps))
    mus[g] <- mu
    sds[g] <- sdv
    out[ix] <- (seg - mu) / sdv
  }
  list(x = out, mean = mus, sd = sds, n_groups = G, group_size = per)
}

#' Graph readout by sum pooling (standard; Xu et al. 2019 for its power)
#' @param H Argument `H`; see Usage.
#' @return A list with `sum`, `mean`, `max`, `n_nodes`, `dim`.
#' @examples
#' V <- c(1, 2, 3, 4, 5, 6, 7, 8)
#' rmorie:::Sumpl(V)
#' @keywords internal
Sumpl <- function(H) {
  Hm <- as.matrix(H)
  if (nrow(Hm) == 0L) stop("H must have at least one node")
  list(sum = colSums(Hm), mean = colMeans(Hm),
       max = apply(Hm, 2, max), n_nodes = nrow(Hm), dim = ncol(Hm))
}

#' Graph isomorphism network aggregation (Xu et al. 2019)
#' @param A Argument `A`; see Usage.
#' @param H Argument `H`; see Usage.
#' @param eps Argument `eps`; see Usage.
#' @return A list with `H`, `eps`, `n_nodes`, `dim`.
#' @examples
#' A <- rbind(c(0, 1, 0), c(1, 0, 1), c(0, 1, 0))
#' H <- matrix(1:6, 3, 2)
#' rmorie:::Ginagg(A, H, eps = 0.1)
#' @keywords internal
Ginagg <- function(A, H, eps = 0) {
  Am <- as.matrix(A)
  Hm <- as.matrix(H)
  n <- nrow(Hm)
  if (nrow(Am) != n || ncol(Am) != n) stop("A must be n x n matching H")
  e <- as.numeric(eps)
  list(H = (1 + e) * Hm + Am %*% Hm, eps = e, n_nodes = n, dim = ncol(Hm))
}

#' morie_unclr_sym_norm
#'
#' A step of the unclr implementation. Called by \code{Lgcnprop}, \code{Sgcprop}.
#' See the file header for the source the module follows.
#' it follows.
#'
#' @param A A matrix; passed to \code{nrow}.
#' @param self_loops A flag; the body branches on it.
#' @return A numeric value.
#' @export
#' @examples
#' A <- rbind(c(0, 1, 0), c(1, 0, 1), c(0, 1, 0))
#' morie_unclr_sym_norm(A, self_loops = TRUE)
#' @keywords internal
morie_unclr_sym_norm <- function(A, self_loops) {
  n <- nrow(A)
  M <- A + if (self_loops) diag(1, n) else 0
  deg <- rowSums(M)
  if (any(deg <= 0)) stop("a node has non-positive degree; cannot normalise")
  M / sqrt(outer(deg, deg))
}

#' Simplified graph convolution propagation (Wu et al. 2019, SGC)
#' @param A Argument `A`; see Usage.
#' @param X Argument `X`; see Usage.
#' @param K Argument `K`; see Usage.
#' @return A list with `X`, `K`, `n_nodes`, `dim`.
#' @examples
#' A <- rbind(c(0, 1, 0), c(1, 0, 1), c(0, 1, 0))
#' X <- matrix(1:6, 3, 2)
#' rmorie:::Sgcprop(A, X, K = 2)
#' @keywords internal
Sgcprop <- function(A, X, K) {
  S <- morie_unclr_sym_norm(as.matrix(A), TRUE)
  out <- as.matrix(X)
  for (i in seq_len(as.integer(K))) out <- S %*% out
  list(X = out, K = as.integer(K), n_nodes = nrow(S), dim = ncol(out))
}

#' LightGCN layer combination (He et al. 2020)
#' @param A Argument `A`; see Usage.
#' @param E Argument `E`; see Usage.
#' @param K Argument `K`; see Usage.
#' @param alpha Argument `alpha`; see Usage.
#' @return A list with `E`, `K`, `alpha`, `n_nodes`, `dim`.
#' @examples
#' set.seed(1)
#' A <- rbind(c(0, 1, 0), c(1, 0, 1), c(0, 1, 0))
#' E <- matrix(rnorm(6), 3, 2)
#' rmorie:::Lgcnprop(A, E, K = 2)
#' @keywords internal
Lgcnprop <- function(A, E, K, alpha = NULL) {
  S <- morie_unclr_sym_norm(as.matrix(A), FALSE)
  K <- as.integer(K)
  w <- if (is.null(alpha)) rep(1 / (K + 1), K + 1) else as.numeric(alpha)
  if (length(w) != K + 1L)
    stop("alpha must give one weight per layer including layer 0")
  cur <- as.matrix(E)
  acc <- w[1] * cur
  for (k in seq_len(K)) {
    cur <- S %*% cur
    acc <- acc + w[k + 1L] * cur
  }
  list(E = acc, K = K, alpha = w, n_nodes = nrow(acc), dim = ncol(acc))
}

#' LinUCB arm scores (Li et al. 2010)
#' @param x Argument `x`; see Usage.
#' @param theta Argument `theta`; see Usage.
#' @param Ainv Argument `Ainv`; see Usage.
#' @param alpha Argument `alpha`; see Usage.
#' @return A list with `score`, `mean`, `bonus`, `arm`, `n_arms`, `alpha`.
#' @examples
#' rmorie:::Linucb(x = 5L, theta = c(1, 2, 3, 4, 5, 6, 7, 8), Ainv = c(1, 2, 3, 4, 5, 6, 7, 8))
#' @keywords internal
Linucb <- function(x, theta, Ainv, alpha = 1) {
  xv <- as.numeric(x)
  Th <- as.matrix(theta)
  d <- length(xv)
  if (ncol(Th) != d) stop("each theta must match the context dimension")
  if (length(Ainv) != nrow(Th)) stop("need one inverse design matrix per arm")
  na <- nrow(Th)
  mean_ <- numeric(na)
  bonus <- numeric(na)
  for (aa in seq_len(na)) {
    Ai <- as.matrix(Ainv[[aa]])
    if (nrow(Ai) != d || ncol(Ai) != d) stop("each Ainv must be d x d")
    q <- as.numeric(t(xv) %*% Ai %*% xv)
    if (q < 0) stop("x' Ainv x is negative; Ainv must be positive semidefinite")
    mean_[aa] <- sum(Th[aa, ] * xv)
    bonus[aa] <- as.numeric(alpha) * sqrt(q)
  }
  score <- mean_ + bonus
  list(score = score, mean = mean_, bonus = bonus,
       arm = which.max(score) - 1L, n_arms = na, alpha = as.numeric(alpha))
}

#' Structured state-space convolution kernel (Gu, Goel & Re 2022, S4)
#' @param A Argument `A`; see Usage.
#' @param B Argument `B`; see Usage.
#' @param C Argument `C`; see Usage.
#' @param L Argument `L`; see Usage.
#' @return A list with `K`, `L`, `state_dim`.
#' @examples
#' rmorie:::Ssmk(A = c(1, 2, 3, 4, 5, 6, 7, 8), B = c(1, 2, 3, 4, 5, 6, 7, 8),
#'   C = c(1, 2, 3, 4, 5, 6, 7, 8), L = 5L)
#' @keywords internal
Ssmk <- function(A, B, C, L) {
  Am <- as.matrix(A)
  Bv <- as.numeric(B)
  Cv <- as.numeric(C)
  n <- nrow(Am)
  if (length(Bv) != n || length(Cv) != n)
    stop("B and C must match the state dimension of A")
  K <- numeric(as.integer(L))
  v <- Bv
  for (l in seq_len(as.integer(L))) {
    K[l] <- sum(Cv * v)
    v <- as.numeric(Am %*% v)
  }
  list(K = K, L = as.integer(L), state_dim = n)
}

#' Causal convolution y_t = sum_l K_l x_\{t-l\} (standard)
#' @param K Argument `K`; see Usage.
#' @param x Argument `x`; see Usage.
#' @return A vector.
#' @examples
#' V <- c(1, 2, 3, 4, 5, 6, 7, 8)
#' rmorie:::Ssmconv(V, V)
#' @keywords internal
Ssmconv <- function(K, x) {
  Kv <- as.numeric(K)
  xv <- as.numeric(x)
  vapply(seq_along(xv), function(t) {
    nl <- min(t, length(Kv))
    sum(Kv[seq_len(nl)] * xv[t - seq_len(nl) + 1L])
  }, numeric(1))
}

#' Dominant periods from the amplitude spectrum (Wu et al. 2023, TimesNet)
#' @param x Argument `x`; see Usage.
#' @param k Argument `k`; see Usage.
#' @return A list with `frequency`, `period`, `amplitude`, `spectrum`, `n`.
#' @examples
#' V <- c(1, 2, 3, 4, 5, 6, 7, 8)
#' rmorie:::Fftperiod(V)
#' @keywords internal
Fftperiod <- function(x, k = 1) {
  xv <- as.numeric(x)
  n <- length(xv)
  if (n < 4L) stop("need at least 4 observations")
  amps <- morie_unclr_dft_amp(xv)
  half <- n %/% 2L
  cand <- seq_len(half)
  kk <- as.integer(k)
  if (kk < 1L || kk > length(cand)) stop(sprintf("k must lie in 1..%d", length(cand)))
  ord <- cand[order(-amps[cand + 1L], cand)][seq_len(kk)]
  list(frequency = ord, period = n / ord, amplitude = amps[ord + 1L],
       spectrum = amps[seq_len(half + 1L)], n = n)
}

#' Series decomposition and autocorrelation (Wu et al. 2021, Autoformer)
#' @param x Argument `x`; see Usage.
#' @param kernel Argument `kernel`; see Usage.
#' @return A list with `trend`, `seasonal`, `acf`, `kernel`, `n`.
#' @examples
#' set.seed(5)
#' x <- sin(2 * pi * (1:60) / 12) + 0.1 * (1:60) / 10 + rnorm(60, 0, 0.1)
#' str(rmorie:::Serdecomp(x, kernel = 5), max.level = 1)
#' @keywords internal
Serdecomp <- function(x, kernel) {
  xv <- as.numeric(x)
  n <- length(xv)
  kk <- as.integer(kernel)
  if (kk < 1L || kk %% 2L == 0L) stop("kernel must be a positive odd integer")
  if (kk > n) stop("kernel must not exceed the series length")
  h <- kk %/% 2L
  pad <- c(rep(xv[1], h), xv, rep(xv[n], h))
  trend <- vapply(seq_len(n), function(i) mean(pad[i:(i + kk - 1L)]), numeric(1))
  seas <- xv - trend
  m <- mean(seas)
  den <- sum((seas - m)^2)
  acf <- vapply(seq_len(n) - 1L, function(lag) {
    if (den <= 0) return(NaN)
    sum((seas[seq_len(n - lag)] - m) * (seas[seq_len(n - lag) + lag] - m)) / den
  }, numeric(1))
  list(trend = trend, seasonal = seas, acf = acf, kernel = kk, n = n)
}
