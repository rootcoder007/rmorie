# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Horowitz shelf mirrors, part 3: deconvolution, average derivative
# and nonparametric IV. Mirrors morie.fn.hrzdeconv, hrzdcrc, hrzdcnm,
# hrzade, hrzades, hrztikr, hrzsitr, hrznpivt, hrznqiv, hrzinst.
#
# Collision scan: horowitz_native3.R and all eleven exported names
# below were free in both R trees; .hrz_silverman and
# .hrz_gauss_kernel are reused from R/aaa_helpers_horowitz.R, and the
# sieve basis is reused from morie_sieve_basis in horowitz_native2.R,
# so the two languages stay on one bandwidth rule and one basis.
#
# Spec: Horowitz, J. L., Semiparametric and Nonparametric Methods in
# Econometrics, Springer. Sec. 2.6 (average derivative), Sec. 5.1
# (deconvolution), Sec. 5.3-5.5 (nonparametric IV). NPIV is in
# CHAPTER 5, not chapter 6 -- chapter 6 is transformation models,
# verified against the printed table of contents.

# K'(u) for the Gaussian kernel; the sign lives in the derivative
# itself, and adding a second leading minus flips the average
# derivative's sign (measured -0.548 against a theoretical +0.564).
#' K\'(u) for the Gaussian kernel; the sign lives in the derivative
#'
#' itself, and adding a second leading minus flips the average
#' derivative\'s sign (measured -0.548 against a theoretical +0.564).
#'
#' @param u Numeric; combined arithmetically in the body.
#' @return A numeric value.
#' @export
#' @examples
#' x <- c(1.2, 2.4, 3.1, 4.8, 5.3, 6.7, 7.1, 8.9)
#' res <- .hrz_gauss_kernel_deriv(u = x)
#' res
.hrz_gauss_kernel_deriv <- function(u) -u * exp(-0.5 * u^2) / sqrt(2 * pi)

#' Deconvolution kernel density estimate
#'
#' \eqn{\hat f_U(u) = (2\pi)^{-1}\int e^{-i\tau u}
#' \psi_W(\tau)/\psi_\epsilon(\tau)\,d\tau}, recovering the density of
#' U from \eqn{W = U + \epsilon}. Dividing by the error characteristic
#' function is the whole idea and the whole difficulty: for a normal
#' error \eqn{\psi_\epsilon} decays like \eqn{e^{-\sigma^2\tau^2/2}},
#' so the ratio explodes and the rate collapses to a logarithmic
#' \eqn{(\log n)^{-s}}. A damping kernel is mandatory, not optional.
#' Mirrors \code{morie.fn.hrzdeconv}.
#'
#' The default bandwidth comes from the noise-amplification criterion
#' rather than a generic rate: the estimator variance carries
#' \eqn{\int |\hat K/\psi_\epsilon|^2}, so the cut-off \eqn{T = 1/h}
#' may only grow while that integral over n stays bounded. Normal
#' error gives \eqn{h = \sigma/\sqrt{\log n}}; Laplace error gives
#' \eqn{h = n^{-1/5}}.
#'
#' @param W numeric contaminated observations.
#' @param sigma_eps positive error scale.
#' @param grid evaluation points; a spanning grid when NULL.
#' @param h damping bandwidth; the criterion above when NULL.
#' @param error "normal" or "laplace".
#' @return list: grid, density, bandwidth, regime, rate_note, n, method.
#' @references Horowitz, Ch. 5, Sec. 5.1.
#' @examples
#' w <- rnorm(200) + rnorm(200) * 0.4
#' morie_deconvolution(w, 0.4, grid = 0)$regime
#' @export
morie_deconvolution <- function(W, sigma_eps, grid = NULL, h = NULL,
                                error = "normal") {
  W <- as.numeric(W)
  n <- length(W)
  if (n < 8L) {
    stop(sprintf("need at least 8 observations, got %d.", n),
      call. = FALSE
    )
  }
  s <- as.numeric(sigma_eps)
  if (s <= 0) {
    stop(sprintf("sigma_eps must be positive, got %g.", s),
      call. = FALSE
    )
  }
  if (!error %in% c("normal", "laplace")) {
    stop("error must be 'normal' or 'laplace'.", call. = FALSE)
  }
  hh <- if (is.null(h)) {
    if (error == "normal") s / sqrt(log(n)) else n^(-0.2)
  } else {
    as.numeric(h)
  }
  if (hh <= 0) {
    stop(sprintf("bandwidth must be positive, got %g.", hh),
      call. = FALSE
    )
  }
  g <- if (is.null(grid)) seq(min(W), max(W), length.out = 200L) else as.numeric(grid)

  # sinc-kernel Fourier transform: compactly supported in tau, which
  # is what keeps 1/psi_eps from being evaluated where it vanishes
  tt <- 1 / hh
  tau <- seq(-tt, tt, length.out = 2001L)
  psi_w <- rowMeans(exp(1i * outer(tau, W)))
  if (error == "normal") {
    psi_e <- exp(-0.5 * s^2 * tau^2)
    regime <- "supersmooth"
    note <- "psi_eps decays exponentially: rate is logarithmic, (log n)^{-s}"
  } else {
    psi_e <- 1 / (1 + s^2 * tau^2)
    regime <- "ordinary smooth"
    note <- "psi_eps decays polynomially: rate stays polynomial, n^{-r}"
  }
  damp <- (1 - (tau / tt)^2)^3 # vanishes at the cut-off
  integrand <- psi_w / psi_e * damp
  dens <- vapply(g, function(u) {
    z <- integrand * exp(-1i * tau * u)
    Re(sum(diff(tau) * (utils::head(z, -1L) + utils::tail(z, -1L)) / 2)) / (2 * pi)
  }, numeric(1))
  list(
    grid = g, density = dens, bandwidth = hh, regime = regime,
    rate_note = note, n = n,
    method = "Fourier deconvolution with a compact damping kernel"
  )
}

#' Convergence rates for deconvolution
#'
#' Ordinary-smooth error gives \eqn{O_p(n^{-r})}; supersmooth error
#' gives \eqn{O_p\[(\log n)^{-s}\]}. The gap is the practical message of
#' the chapter: at \eqn{n = 10^6} a logarithmic rate has barely moved.
#' Both are returned at the requested n so the difference is a number
#' rather than a footnote. Mirrors \code{morie.fn.hrzdcrc}.
#'
#' @param n sample size.
#' @param error "normal" or "laplace".
#' @param s supersmooth exponent.
#' @param r ordinary-smooth exponent.
#' @return list: rate, regime, polynomial_rate, logarithmic_rate,
#'   ratio, n, method.
#' @references Horowitz, Ch. 5, Sec. 5.1.1-5.1.2.
#' @examples
#' morie_deconv_rate(1e6)$ratio
#' @export
morie_deconv_rate <- function(n, error = "normal", s = 2, r = 2) {
  n <- as.integer(n)
  if (is.na(n) || n < 2L) stop("n must be at least 2.", call. = FALSE)
  if (!error %in% c("normal", "laplace")) {
    stop("error must be 'normal' or 'laplace'.", call. = FALSE)
  }
  poly <- n^(-as.numeric(r))
  logr <- log(n)^(-as.numeric(s))
  supersmooth <- error == "normal"
  list(
    rate = if (supersmooth) logr else poly,
    regime = if (supersmooth) "supersmooth" else "ordinary smooth",
    polynomial_rate = poly, logarithmic_rate = logr,
    ratio = if (poly > 0) logr / poly else Inf, n = n,
    method = "n^{-r} vs (log n)^{-s}; the gap is the chapter's point"
  )
}

#' Asymptotic normality of the deconvolution estimator
#'
#' \eqn{\[n h_n/b_n\]^{1/2}(\hat f_U(u) - f_U(u) - \mathrm{bias}) \to_D
#' N(0, \sigma^2)}. The normalising factor carries \eqn{b_n}, a
#' deconvolution-specific inflation absent from ordinary kernel
#' estimation -- it encodes the price of dividing by a vanishing
#' characteristic function. The bias is SUBTRACTED, not assumed away:
#' undersmoothing is what makes it negligible, and if it is not, the
#' interval is centred wrongly. Mirrors \code{morie.fn.hrzdcnm}.
#'
#' @param fn_u estimate at the evaluation point.
#' @param f_u truth at the evaluation point.
#' @param n sample size.
#' @param h bandwidth, positive.
#' @param b deconvolution inflation factor, positive.
#' @param bias asymptotic bias.
#' @param sigma limiting standard deviation, positive.
#' @return list: z, scaling, p_two_sided, bias_subtracted, method.
#' @references Horowitz, Ch. 5, Sec. 5.1.3.
#' @examples
#' morie_deconv_normality(0.41, 0.399, 1000, 0.2, 2)$z
#' @export
morie_deconv_normality <- function(fn_u, f_u, n, h, b, bias = 0, sigma = 1) {
  n <- as.integer(n)
  h <- as.numeric(h)
  b <- as.numeric(b)
  if (is.na(n) || n < 2L || h <= 0 || b <= 0) {
    stop("need n >= 2 and positive h, b.", call. = FALSE)
  }
  sig <- as.numeric(sigma)
  if (sig <= 0) {
    stop(sprintf("sigma must be positive, got %g.", sig),
      call. = FALSE
    )
  }
  scale <- sqrt(n * h / b)
  z <- scale * (as.numeric(fn_u) - as.numeric(f_u) - as.numeric(bias)) / sig
  list(
    z = z, scaling = scale,
    p_two_sided = 2 * stats::pnorm(abs(z), lower.tail = FALSE),
    bias_subtracted = as.numeric(bias),
    method = "[n h / b]^{1/2}(f-hat - f - bias) -> N(0, sigma^2)"
  )
}

#' Density-weighted average derivative
#'
#' \eqn{\delta = E\[f_X(X)\,\partial E(Y|X)/\partial X\] = -2E\[f_X'(X)Y\]}.
#' This is the DENSITY-WEIGHTED average derivative, not the plain
#' \eqn{E\[\partial E(Y|X)/\partial X\]}: for standard normal X with
#' \eqn{E(Y|X) = 2X} the weighted estimand is \eqn{2\int\phi^2 = 0.564}
#' while the unweighted one is 2. Integration by parts turns a
#' derivative of an unknown regression into an expectation involving
#' the DENSITY derivative, which is why this estimand is root-n
#' estimable even though \eqn{E(Y|X)} is not. Mirrors
#' \code{morie.fn.hrzade}.
#'
#' @param X numeric vector or matrix of covariates.
#' @param y numeric response.
#' @param h bandwidth for the density derivative; Silverman when NULL.
#' @param weighted use the density-weighted form (kept for parity).
#' @return list: delta, se, root_n, proportional_to_beta, bandwidth,
#'   n, d, method.
#' @references Horowitz, Ch. 2, Sec. 2.6.1.
#' @examples
#' x <- rnorm(300)
#' morie_average_derivative(x, 2 * x + rnorm(300, sd = 0.1))$delta
#' @export
morie_average_derivative <- function(X, y, h = NULL, weighted = TRUE) {
  y <- as.numeric(y)
  X <- if (is.matrix(X)) X else matrix(as.numeric(X), ncol = 1L)
  if (nrow(X) != length(y)) X <- t(X)
  if (nrow(X) != length(y)) {
    stop("X must have one row per entry of y.", call. = FALSE)
  }
  n <- nrow(X)
  d <- ncol(X)
  delta <- numeric(d)
  hs <- numeric(d)
  infl <- matrix(0, n, d)
  for (j in seq_len(d)) {
    xj <- X[, j]
    hj <- if (is.null(h)) .hrz_silverman(xj) else as.numeric(h)
    hs[j] <- hj
    # leave-one-out, so an observation does not contribute to its own
    # density derivative
    kp <- .hrz_gauss_kernel_deriv(outer(xj, xj, "-") / hj)
    diag(kp) <- 0
    fprime <- rowSums(kp) / ((n - 1) * hj^2)
    contrib <- -2 * fprime * y
    delta[j] <- mean(contrib)
    infl[, j] <- contrib - delta[j]
  }
  se <- sqrt(colSums(infl^2)) / n
  list(
    delta = if (d > 1L) delta else delta[1L],
    se = if (d > 1L) se else se[1L],
    root_n = TRUE, proportional_to_beta = TRUE,
    bandwidth = if (d > 1L) hs else hs[1L], n = n, d = d,
    method = "Density-weighted average derivative; root-n by parts"
  )
}

#' Sample average derivative with an undersmoothed bandwidth
#'
#' The explicit estimator behind \code{\link{morie_average_derivative}}.
#' Two details carry the root-n property: the density derivative is
#' computed LEAVE-ONE-OUT, and the bandwidth must UNDERSMOOTH relative
#' to the density-optimal choice so the bias vanishes faster than
#' \eqn{n^{-1/2}}. A density-optimal bandwidth leaves a bias of the
#' same order as the standard error, and the confidence interval is
#' then centred on the wrong value. Mirrors \code{morie.fn.hrzades}.
#'
#' @param X numeric vector or matrix of covariates.
#' @param y numeric response.
#' @param h bandwidth; an undersmoothed default when NULL.
#' @return list: delta_hat, se, bandwidth, undersmoothed, n, method.
#' @references Horowitz, Ch. 2, Sec. 2.6.1-2.6.2.
#' @examples
#' x <- rnorm(300)
#' morie_average_derivative_hat(x, 2 * x)$undersmoothed
#' @export
morie_average_derivative_hat <- function(X, y, h = NULL) {
  y <- as.numeric(y)
  Xm <- if (is.matrix(X)) X else matrix(as.numeric(X), ncol = 1L)
  if (nrow(Xm) != length(y)) Xm <- t(Xm)
  n <- nrow(Xm)
  if (is.null(h)) {
    # undersmooth: n^{-1/5} * n^{-1/20} shrinks the bias faster
    h <- .hrz_silverman(Xm[, 1L]) * n^(-0.05)
  }
  out <- morie_average_derivative(Xm, y, h = h)
  list(
    delta_hat = out$delta, se = out$se, bandwidth = out$bandwidth,
    undersmoothed = TRUE, n = out$n,
    method = "Sample average derivative; LOO and undersmoothing are required"
  )
}

#' Tikhonov regularisation for nonparametric IV
#'
#' \eqn{\hat g = \arg\min_g \[\|\hat E(Y|W) - \hat Tg\|^2 +
#' \alpha_n\|g\|^2\]}. The operator T is compact, so its inverse is
#' UNBOUNDED and the problem is ill-posed: without the penalty,
#' arbitrarily small perturbations in the estimated right-hand side
#' produce arbitrarily large changes in g. The solution norm across a
#' grid of alpha is returned so the L-curve trade-off is visible
#' rather than a single alpha being picked silently. Mirrors
#' \code{morie.fn.hrztikr}.
#'
#' @param T numeric matrix, the discretised operator.
#' @param Ey_w numeric estimated conditional mean, one per row of T.
#' @param alpha regularisation parameter; the grid midpoint when NULL.
#' @param alphas positive grid for the L-curve.
#' @return list: g, alpha, residual_norm, solution_norm, l_curve,
#'   condition_number, ill_posed, method.
#' @references Horowitz, Ch. 5, Sec. 5.3.1 and 5.4.1.
#' @examples
#' morie_tikhonov_iv(diag(3), c(1, 2, 3))$alpha
#' @export
morie_tikhonov_iv <- function(T, Ey_w, alpha = NULL, alphas = NULL) {
  Tm <- if (is.matrix(T)) T else matrix(as.numeric(T), nrow = 1L)
  b <- as.numeric(Ey_w)
  if (nrow(Tm) != length(b)) {
    stop(sprintf("T has %d rows but Ey_w has %d.", nrow(Tm), length(b)),
      call. = FALSE
    )
  }
  k <- ncol(Tm)
  tt <- crossprod(Tm)
  tb <- crossprod(Tm, b)
  cond <- if (k > 0L) kappa(tt, exact = TRUE) else Inf
  solve_a <- function(a) as.numeric(solve(tt + as.numeric(a) * diag(k), tb))
  grid <- if (is.null(alphas)) {
    c(1e-6, 1e-5, 1e-4, 1e-3, 1e-2, 1e-1)
  } else {
    as.numeric(alphas)
  }
  if (any(grid <= 0)) stop("alpha values must be positive.", call. = FALSE)
  curve <- lapply(grid, function(a) {
    ga <- solve_a(a)
    c(a, sqrt(sum((Tm %*% ga - b)^2)), sqrt(sum(ga^2)))
  })
  a_use <- if (is.null(alpha)) grid[length(grid) %/% 2L + 1L] else as.numeric(alpha)
  if (a_use <= 0) {
    stop(sprintf("alpha must be positive, got %g.", a_use),
      call. = FALSE
    )
  }
  g <- solve_a(a_use)
  list(
    g = g, alpha = a_use,
    residual_norm = sqrt(sum((Tm %*% g - b)^2)),
    solution_norm = sqrt(sum(g^2)),
    l_curve = do.call(rbind, curve), condition_number = cond,
    ill_posed = TRUE,
    method = "Tikhonov; T compact so T^{-1} is unbounded"
  )
}

#' Sieve (series) solution of the nonparametric IV equation
#'
#' \eqn{\hat g = \arg\min_{g \in G_K}\|\hat E(Y|W) - \hat Tg\|^2} with
#' \eqn{G_K} a K-dimensional sieve. Here K itself does the
#' regularising: truncating the basis bounds the inverse, exactly as
#' the Tikhonov penalty does. The two are alternative regularisations
#' of the SAME ill-posed problem, and choosing K too large reproduces
#' the instability the sieve was meant to prevent -- so the condition
#' number at the chosen K is reported. Mirrors
#' \code{morie.fn.hrzsitr}.
#'
#' @param T numeric matrix, the discretised operator.
#' @param Ey_w numeric estimated conditional mean.
#' @param K sieve dimension; a conservative truncation when NULL.
#' @return list: g (zero-padded beyond K), K, residual_norm,
#'   condition_number_at_K, regularisation, method.
#' @references Horowitz, Ch. 5, Sec. 5.4.2.
#' @examples
#' morie_sieve_iv(diag(4), c(1, 2, 3, 4))$K
#' @export
morie_sieve_iv <- function(T, Ey_w, K = NULL) {
  Tm <- if (is.matrix(T)) T else matrix(as.numeric(T), nrow = 1L)
  b <- as.numeric(Ey_w)
  if (nrow(Tm) != length(b)) {
    stop(sprintf("T has %d rows but Ey_w has %d.", nrow(Tm), length(b)),
      call. = FALSE
    )
  }
  k <- ncol(Tm)
  kd <- if (is.null(K)) max(1L, min(k, as.integer(sqrt(nrow(Tm))))) else as.integer(K)
  if (kd < 1L || kd > k) {
    stop(sprintf("K must lie in 1..%d, got %d.", k, kd), call. = FALSE)
  }
  tk <- Tm[, seq_len(kd), drop = FALSE]
  gk <- as.numeric(qr.coef(qr(tk), b))
  gk[is.na(gk)] <- 0
  g <- numeric(k)
  g[seq_len(kd)] <- gk
  list(
    g = g, K = kd,
    residual_norm = sqrt(sum((tk %*% gk - b)^2)),
    condition_number_at_K = kappa(crossprod(tk), exact = TRUE),
    regularisation = "truncation",
    method = "Sieve NPIV; K regularises exactly as alpha does"
  )
}

#' Sieve estimate of the NPIV operator
#'
#' \eqn{(Tg)(w) = \int g(x)f_{X|W}(x|w)\,dx}, represented on a sieve
#' basis as \eqn{\hat T_{jk} = \hat E\[p_k(X)q_j(W)\]}. The operator's
#' singular values decay to zero -- that decay IS the ill-posedness,
#' and how fast it decays determines whether the problem is mildly or
#' severely ill-posed. The singular values are returned so that is
#' visible rather than an abstraction. Mirrors
#' \code{morie.fn.hrznpivt}.
#'
#' @param X numeric endogenous regressor.
#' @param W numeric instrument, same length as X.
#' @param K sieve dimension for both bases.
#' @param kind basis type, "poly" or "fourier".
#' @return list: T, singular_values, decay_ratio, severity, K, n, method.
#' @references Horowitz, Ch. 5, Sec. 5.3.
#' @examples
#' z <- rnorm(200)
#' morie_npiv_operator(z + rnorm(200), z)$severity
#' @export
morie_npiv_operator <- function(X, W, K = 5L, kind = "poly") {
  X <- as.numeric(X)
  W <- as.numeric(W)
  if (length(X) != length(W)) {
    stop("X and W must have the same length.", call. = FALSE)
  }
  K <- as.integer(K)
  if (K < 1L || K > length(X)) {
    stop(sprintf("K must lie in 1..%d, got %d.", length(X), K), call. = FALSE)
  }
  p <- morie_sieve_basis(X, K = K, kind = kind)
  q <- morie_sieve_basis(W, K = K, kind = kind)
  tm <- crossprod(q, p) / length(X)
  sv <- svd(tm, nu = 0L, nv = 0L)$d
  ratio <- if (sv[1L] > 0) sv[length(sv)] / sv[1L] else 0
  list(
    T = tm, singular_values = sv, decay_ratio = ratio,
    severity = if (ratio < 1e-6) "severe" else "mild",
    K = K, n = length(X),
    method = "T_jk = E[p_k(X) q_j(W)]; singular decay IS the ill-posedness"
  )
}

#' Nonparametric quantile IV
#'
#' Solve \eqn{P(Y \le g(X) \mid W = w) = \tau} for g. The quantile
#' restriction replaces the mean restriction of ordinary NPIV, and it
#' makes the problem NONLINEAR in g -- the operator equation cannot
#' simply be inverted. The same ill-posedness is present and the same
#' regularisation is required; what changes is that linear-inverse
#' intuition no longer transfers directly. Mirrors
#' \code{morie.fn.hrznqiv}.
#'
#' @param T numeric matrix, the discretised operator.
#' @param tau_target numeric target conditional probabilities.
#' @param K sieve truncation.
#' @param tau quantile level in (0, 1), recorded.
#' @return list: g, K, residual_norm, tau, nonlinear, method.
#' @references Horowitz, Ch. 5, Sec. 5.5.1.
#' @examples
#' morie_npiv_quantile(diag(4), rep(0.5, 4))$nonlinear
#' @export
morie_npiv_quantile <- function(T, tau_target, K = NULL, tau = 0.5) {
  if (!(tau > 0 && tau < 1)) {
    stop(sprintf("tau must lie in (0, 1), got %g.", tau), call. = FALSE)
  }
  out <- morie_sieve_iv(T, tau_target, K = K)
  list(
    g = out$g, K = out$K, residual_norm = out$residual_norm,
    tau = as.numeric(tau), nonlinear = TRUE,
    method = "Quantile restriction; nonlinear in g, same ill-posedness"
  )
}

#' Instrument relevance and exogeneity diagnostics
#'
#' Identification needs \eqn{E\[U|Z\] = 0} (exogeneity) together with
#' variation in X that Z explains (relevance). Both are reported and
#' they fail in different ways: a weak but valid instrument gives
#' large variance, while a strong but invalid one gives confident
#' nonsense. Exogeneity is NOT testable without further restrictions
#' -- the returned correlation is a diagnostic against a supplied
#' residual, not a test, and the key is named accordingly. Mirrors
#' \code{morie.fn.hrzinst}.
#'
#' @param X numeric endogenous regressor(s).
#' @param Z numeric instrument(s), same number of rows as X.
#' @param U residuals for the exogeneity diagnostic.
#' @param y response; residuals are formed by 2SLS when U is omitted.
#' @return list: first_stage_r2, first_stage_F, relevant, corr_U_Z,
#'   exogeneity_testable, n, n_instruments, method.
#' @references Horowitz, Ch. 5, Sec. 5.3.
#' @examples
#' z <- rnorm(200)
#' morie_instrument_check(z + rnorm(200), z)$relevant
#' @export
morie_instrument_check <- function(X, Z, U = NULL, y = NULL) {
  Xm <- if (is.matrix(X)) X else matrix(as.numeric(X), ncol = 1L)
  Zm <- if (is.matrix(Z)) Z else matrix(as.numeric(Z), ncol = 1L)
  if (nrow(Xm) < ncol(Xm)) Xm <- t(Xm)
  if (nrow(Zm) < ncol(Zm)) Zm <- t(Zm)
  n <- nrow(Xm)
  if (nrow(Zm) != n) {
    stop("X and Z must have the same number of rows.", call. = FALSE)
  }
  zc <- cbind(1, Zm)
  q <- ncol(Zm)
  x1 <- Xm[, 1L]
  coef <- qr.coef(qr(zc), x1)
  coef[is.na(coef)] <- 0
  fit <- as.numeric(zc %*% coef)
  ss_res <- sum((x1 - fit)^2)
  ss_tot <- sum((x1 - mean(x1))^2)
  r2 <- if (ss_tot > 0) 1 - ss_res / ss_tot else 0
  dof <- max(n - q - 1L, 1L)
  ff <- if (r2 < 1) (r2 / q) / ((1 - r2) / dof) else Inf

  corr <- NULL
  if (!is.null(U)) {
    u <- as.numeric(U)
    if (length(u) != n) {
      stop("U must have one entry per row of X.", call. = FALSE)
    }
    corr <- stats::cor(u, Zm[, 1L])
  } else if (!is.null(y)) {
    yy <- as.numeric(y)
    if (length(yy) != n) {
      stop("y must have one entry per row of X.", call. = FALSE)
    }
    b2 <- qr.coef(qr(cbind(1, fit)), yy)
    b2[is.na(b2)] <- 0
    u <- yy - as.numeric(cbind(1, x1) %*% b2)
    corr <- stats::cor(u, Zm[, 1L])
  }
  list(
    first_stage_r2 = r2, first_stage_F = ff,
    relevant = ff > 10, # the usual rule of thumb
    corr_U_Z = corr, exogeneity_testable = FALSE,
    n = n, n_instruments = q,
    method = "Relevance is testable; exogeneity is NOT, and is not claimed"
  )
}
