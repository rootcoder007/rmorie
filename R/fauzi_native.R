# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Kernel distribution-function estimation, after Fauzi, R. R. and
# Maesono, Y. (2023), "Statistical Inference Based on Kernel
# Distribution Function Estimators", SpringerBriefs in Statistics.
# Equation numbers are the book's and were read off the PDF; the
# distilled text file in the reference library drops the Jacobian
# factor in the boundary-free density and truncates (4.24), so it was
# not used as the source.
#
# R mirror of morie.fn.fz{kde,mise,gkde,kdfe,bfkd,cs1,cs2,b1t,b2t,
# b3t,mrln,mr2,t43,t44,t45,t46,c1c6,kqe,amse,koc,mkrn,l31}. The
# exports are prefixed morie_fauzi_ throughout; note that
# morie_kernel_quantile already exists in horowitz_native2.R and is a
# DIFFERENT estimator (a conditional quantile regression), which is
# why the one here is morie_fauzi_kernel_quantile.
#
# The organising problem of the book is the BOUNDARY. A symmetric
# kernel placed near the edge of a bounded support puts mass outside
# it, and the resulting bias is O(h) there against O(h^2) in the
# interior -- it does not vanish at the same rate. Every construction
# below is a way around that: gamma kernels whose support matches the
# data, and bijections that move a bounded problem onto the whole
# line and back.

.fz_K <- function(u) exp(-0.5 * u^2) / sqrt(2 * pi)

# W(u) = int_{-inf}^u K(v) dv, the INTEGRATED kernel of (2.2). A
# distribution-function estimator smooths with the kernel's integral,
# not the kernel, which is what makes it continuous where the
# empirical df jumps and why its bias carries f' rather than f''.
.fz_W <- function(u) stats::pnorm(u)

# V(u) = 1 - W(u), the survival counterpart used throughout Ch. 4.
.fz_V <- function(u) 1 - stats::pnorm(u)

.fz_trapz <- function(y, x) sum(diff(x) * (y[-1L] + y[-length(y)])) / 2

.fz_seq <- function(from, to, n) seq(from, to, length.out = n)

# Bandwidth for a DISTRIBUTION-function-type estimator:
# 4^(1/3) sigma n^(-1/3). A cube root, not the fifth root of the
# density rule. Equations (2.3)-(2.4) put the bandwidth in the
# variance at O(h/n) with a NEGATIVE sign -- smoothing REDUCES
# variance here -- rather than at O(1/(nh)), so minimising the MISE
# gives h_opt = (2 r_1 / (n mu_2^2 R(f')))^(1/3), which for a
# Gaussian kernel against a normal reference is 4^(1/3) sigma
# n^(-1/3). Sec. 5.3.2 states the same conclusion in words, citing
# Azzalini (1981). Identical to .morie_kdfe_h in
# aaa_helpers_fauzi.R; kept local so this file stands alone.
.fz_kdfe_h <- function(x) {
  n <- length(x)
  s <- stats::sd(x)
  iq <- diff(stats::quantile(x, c(0.25, 0.75), names = FALSE)) / 1.349
  sigma <- if (iq > 0) min(s, iq) else s
  if (sigma <= 0) sigma <- 1
  4^(1 / 3) * sigma * n^(-1 / 3)
}

# A bijection g from the whole line onto the support, with inverse
# and first two derivatives. The derivatives are what appear in the
# bias coefficients b_1, b_2, b_3 of (4.14), (4.15) and (4.21): the
# transformation does not remove the bias, it makes it computable and
# O(h^2) everywhere including at the edge.
.fz_transform <- function(kind = "log") {
  if (identical(kind, "log")) {
    return(list(g = exp, g_inv = log, dg = exp, d2g = exp,
                support = c(0, Inf), name = "exp/log"))
  }
  if (identical(kind, "identity")) {
    return(list(g = function(z) z, g_inv = function(t) t,
                dg = function(z) rep(1, length(z)),
                d2g = function(z) rep(0, length(z)),
                support = c(-Inf, Inf), name = "identity"))
  }
  stop("kind must be 'log' or 'identity'.", call. = FALSE)
}

# Order-m kernel (Mueller 1991). Higher order buys an O(h^m) bias and
# pays for it by taking NEGATIVE values, so the density estimate can
# go negative and the distribution estimate non-monotone. That is the
# trade the book makes explicit.
.fz_muller <- function(u, m = 4L) {
  m <- as.integer(m)
  if (m == 2L) return(.fz_K(u))
  if (m == 4L) return((3 - u^2) / 2 * .fz_K(u))
  if (m == 6L) return((15 - 10 * u^2 + u^4) / 8 * .fz_K(u))
  stop("m must be 2, 4 or 6.", call. = FALSE)
}

.fz_check_sample <- function(x, min_n = 2L) {
  x <- as.numeric(x)
  if (length(x) < min_n) {
    stop(sprintf("need at least %d observations, got %d.",
                 min_n, length(x)), call. = FALSE)
  }
  x
}

.fz_check_h <- function(h) {
  h <- as.numeric(h)
  if (h <= 0) stop(sprintf("bandwidth must be positive, got %g.", h),
                   call. = FALSE)
  h
}


#' Rosenblatt-Parzen kernel density estimate
#'
#' The baseline whose boundary failure motivates the whole book: a
#' symmetric kernel near the edge of a bounded support leaks mass
#' outside it, so the bias there is `O(h)` and does not vanish at the
#' interior rate.
#'
#' @param x numeric sample.
#' @param grid evaluation points; a data-driven grid if `NULL`.
#' @param h bandwidth; Silverman's rule if `NULL`.
#' @return list: grid, density, bandwidth, mass, interior_bias_order,
#'   boundary_bias_order, boundary_consistent (`FALSE`), n, method.
#' @references Fauzi and Maesono (2023), Ch. 1.
#' @examples
#' morie_fauzi_kde(stats::rexp(200), grid = c(0.5, 1, 2))$density
#' @export
morie_fauzi_kde <- function(x, grid = NULL, h = NULL) {
  xv <- .fz_check_sample(x)
  n <- length(xv)
  if (is.null(h)) {
    sd_x <- stats::sd(xv)
    iqr <- unname(diff(stats::quantile(xv, c(0.25, 0.75), type = 7L)))
    scale <- if (iqr > 0) min(sd_x, iqr / 1.349) else sd_x
    hh <- 1.06 * (if (scale > 0) scale else 1) * n^(-0.2)
  } else {
    hh <- as.numeric(h)
  }
  hh <- .fz_check_h(hh)
  g <- if (is.null(grid)) {
    .fz_seq(min(xv) - 3 * hh, max(xv) + 3 * hh, 200L)
  } else as.numeric(grid)
  dens <- rowSums(.fz_K(outer(g, xv, "-") / hh)) / (n * hh)
  list(grid = g, density = dens, bandwidth = hh,
       mass = .fz_trapz(dens, g),
       interior_bias_order = "O(h^2)",
       boundary_bias_order = "O(h) -- does NOT vanish at the same rate",
       boundary_consistent = FALSE,
       n = n,
       method = paste("Rosenblatt-Parzen KDE; the boundary failure is",
                      "what Ch. 1 and Ch. 4 are for"))
}


#' MISE of a second-order kernel density estimate
#'
#' `MISE = R(K)/(nh) + h^4 mu2^2 R(f'')/4`. The two terms pull in
#' opposite directions, and the balance caps the attainable rate at
#' `n^{-4/5}` -- strictly worse than the parametric `n^{-1}`, and the
#' ceiling that higher-order kernels and gamma kernels try to beat.
#'
#' @param n sample size.
#' @param h bandwidth; the optimiser if `NULL`.
#' @param R_K `int K^2`; the Gaussian value if `NULL`.
#' @param mu2_K `int u^2 K(u) du`.
#' @param R_f2 `int f''^2`.
#' @param sigma retained for signature parity with the Python module.
#' @return list: mise, variance_part, bias_part, h, h_optimal,
#'   mise_optimal, rate_exponent, parametric_rate_exponent,
#'   bandwidth_rate, ceiling_note, n, method.
#' @references Fauzi and Maesono (2023), Ch. 1.
#' @examples
#' morie_fauzi_mise(1000)$h_optimal
#' @export
morie_fauzi_mise <- function(n, h = NULL, R_K = NULL, mu2_K = 1,
                             R_f2 = 1, sigma = 1) {
  nn <- as.integer(n)
  if (is.na(nn) || nn < 2L) {
    stop(sprintf("n must be at least 2, got %s.", format(n)), call. = FALSE)
  }
  rk <- if (is.null(R_K)) 1 / (2 * sqrt(pi)) else as.numeric(R_K)
  m2 <- as.numeric(mu2_K)
  rf <- as.numeric(R_f2)
  if (rk <= 0 || m2 <= 0 || rf <= 0) {
    stop("R_K, mu2_K and R_f2 must all be positive.", call. = FALSE)
  }
  h_opt <- (rk / (nn * m2^2 * rf))^0.2
  hh <- .fz_check_h(if (is.null(h)) h_opt else as.numeric(h))
  var_part <- rk / (nn * hh)
  bias_part <- hh^4 / 4 * m2^2 * rf
  list(mise = var_part + bias_part, variance_part = var_part,
       bias_part = bias_part, h = hh, h_optimal = h_opt,
       mise_optimal = rk / (nn * h_opt) + h_opt^4 / 4 * m2^2 * rf,
       rate_exponent = -0.8, parametric_rate_exponent = -1,
       bandwidth_rate = "h_opt proportional to n^{-1/5}",
       ceiling_note = paste("n^{-4/5} is the best a second-order kernel",
                            "can do for a twice-differentiable density"),
       n = nn,
       method = paste("MISE = R(K)/(nh) + h^4 mu2^2 R(f'')/4;",
                      "the two terms pull opposite ways"))
}


#' Chen gamma-kernel density estimate
#'
#' The gamma kernel's support IS `[0, Inf)`, so no mass ever lands on
#' the negative half-line and the boundary bias simply does not
#' arise: the estimator is consistent at zero, where a
#' Gaussian-kernel estimate is not. `modified = TRUE` applies Fauzi's
#' self-elimination correction, which cancels the leading bias term.
#'
#' @param x non-negative numeric sample.
#' @param grid evaluation points in `[0, Inf)`; data-driven if `NULL`.
#' @param h bandwidth; `sd(x) * n^{-2/5}` if `NULL`.
#' @param modified apply the self-elimination correction.
#' @param a the correction's second-bandwidth multiplier, not 1.
#' @return list: grid, density, bandwidth, modified, a,
#'   boundary_consistent (`TRUE`), bias_order, mass, why_it_works, n,
#'   method.
#' @references Fauzi and Maesono (2023), Ch. 1; Chen (1999).
#' @examples
#' morie_fauzi_gamma_kde(stats::rexp(200), grid = c(0, 1), h = 0.1)$density
#' @export
morie_fauzi_gamma_kde <- function(x, grid = NULL, h = NULL,
                                  modified = FALSE, a = 2) {
  xv <- .fz_check_sample(x)
  n <- length(xv)
  if (any(xv < 0)) {
    stop("gamma kernels need data on [0, infinity).", call. = FALSE)
  }
  hh <- .fz_check_h(if (is.null(h)) stats::sd(xv) * n^(-0.4) else as.numeric(h))
  g <- if (is.null(grid)) .fz_seq(0, max(xv) * 1.2, 200L) else as.numeric(grid)
  if (any(g < 0)) stop("the grid must lie in [0, infinity).", call. = FALSE)
  dens_at <- function(bw) {
    vapply(g, function(v) mean(stats::dgamma(xv, shape = v / bw + 1,
                                             scale = bw)),
           numeric(1))
  }
  base <- dens_at(hh)
  if (modified) {
    av <- as.numeric(a)
    if (av == 1 || av <= 0) {
      stop(sprintf("a must be positive and not 1, got %g.", av), call. = FALSE)
    }
    dens <- (av * base - dens_at(av * hh)) / (av - 1)
    order <- "o(h^2): the leading term is cancelled by self-elimination"
  } else {
    dens <- base
    order <- "O(h)"
  }
  list(grid = g, density = dens, bandwidth = hh,
       modified = isTRUE(modified), a = as.numeric(a),
       boundary_consistent = TRUE, bias_order = order,
       mass = .fz_trapz(dens, g),
       why_it_works = paste("the kernel's support IS [0, infinity), so no",
                            "mass crosses the boundary and no correction",
                            "is needed"),
       n = n,
       method = paste("Chen gamma kernel density, with Fauzi's",
                      "self-elimination modification"))
}


#' Nadaraya kernel distribution-function estimator
#'
#' Equation (2.2): smooth with `W = int K` rather than with `K`. The
#' bias therefore carries `f'`, one derivative lower than the density
#' estimator's `f''`, and the estimate is continuous where the
#' empirical df jumps.
#'
#' The default bandwidth is `sd(x) * n^{-1/3}`, NOT the `n^{-1/5}`
#' density rule. Sec. 5.3.2 of the book is explicit that Azzalini
#' (1981) recommended `c n^{-1/3}` for distribution-function
#' estimation, and the book's own simulations use it. Under the
#' density rule this estimator oversmooths badly enough to lose, in
#' mean squared error, to the step function it exists to improve on.
#'
#' @param x numeric sample.
#' @param grid evaluation points; data-driven if `NULL`.
#' @param h bandwidth; `4^(1/3) sigma n^{-1/3}` if `NULL`.
#' @return list: grid, F_hat, F_empirical, bandwidth, bandwidth_rate,
#'   monotone, bias_term, uses_integrated_kernel, why_over_edf, n,
#'   method.
#' @references Fauzi and Maesono (2023), Eq. (2.2) and Sec. 5.3.2;
#'   Nadaraya (1964); Azzalini, A. (1981), "A note on the estimation
#'   of a distribution function and quantiles by a kernel method",
#'   Biometrika 68:326-328 (reference \[9\] of the book).
#' @examples
#' morie_fauzi_kdfe(stats::rexp(200), grid = c(0.5, 1))$F_hat
#' @export
morie_fauzi_kdfe <- function(x, grid = NULL, h = NULL) {
  xv <- .fz_check_sample(x)
  n <- length(xv)
  hh <- .fz_check_h(if (is.null(h)) .fz_kdfe_h(xv) else as.numeric(h))
  g <- if (is.null(grid)) {
    .fz_seq(min(xv) - 3 * hh, max(xv) + 3 * hh, 200L)
  } else as.numeric(grid)
  f_hat <- rowSums(.fz_W(outer(g, xv, "-") / hh)) / n
  emp <- vapply(g, function(v) mean(xv <= v), numeric(1))
  list(grid = g, F_hat = f_hat, F_empirical = emp, bandwidth = hh,
       bandwidth_rate = "n^{-1/3} (Azzalini), not the n^{-1/5} density rule",
       monotone = all(diff(f_hat) >= -1e-12),
       bias_term = "h^2 mu_2(K) f'(x)/2 + o(h^2): f PRIME, not f double prime",
       uses_integrated_kernel = TRUE,
       why_over_edf = paste("the empirical df is a step function: not",
                            "continuous, not smoothly invertible, and has",
                            "no density"),
       n = n,
       method = paste("Nadaraya KDFE (2.2); smooths with W = integral of K,",
                      "so the bias carries f'"))
}


#' Boundary-free kernel density estimate by bijection
#'
#' Estimates on the transformed scale, where a symmetric kernel is
#' legitimate because the support is unbounded, then maps back:
#' `f~_X(t) = 1/(n h g'(g^{-1}(t))) sum K((g^{-1}(t) - g^{-1}(X_i))/h)`.
#'
#' The factor `1/g'(g^{-1}(t))` is mandatory. Without it the result
#' is a density on the transformed scale, not the original one. (The
#' distilled text file in the reference library omits it; the PDF
#' does not.)
#'
#' @param x numeric sample strictly inside the support.
#' @param grid evaluation points strictly inside the support.
#' @param h bandwidth on the transformed scale.
#' @param transform `"log"` or `"identity"`.
#' @return list: grid, density, bandwidth, transform, jacobian
#'   (`= 1/g'(g^{-1}(t))`), g_prime, mass, boundary_bias_order,
#'   jacobian_note, n, method.
#' @references Fauzi and Maesono (2023), Ch. 4. Transcribed from the PDF.
#' @examples
#' morie_fauzi_boundary_free_kde(stats::rexp(300), grid = c(0.1, 1))$density
#' @export
morie_fauzi_boundary_free_kde <- function(x, grid = NULL, h = NULL,
                                          transform = "log") {
  xv <- .fz_check_sample(x)
  n <- length(xv)
  tr <- .fz_transform(transform)
  lo <- tr$support[1L]
  hi <- tr$support[2L]
  if (any(xv <= lo) || any(xv >= hi)) {
    stop(sprintf("the sample must lie strictly inside the support for the %s transformation.",
                 tr$name), call. = FALSE)
  }
  z <- tr$g_inv(xv)
  hh <- .fz_check_h(if (is.null(h)) stats::sd(z) * n^(-0.2) else as.numeric(h))
  g <- if (is.null(grid)) {
    .fz_seq(unname(stats::quantile(xv, 0.02, type = 7L)),
            unname(stats::quantile(xv, 0.98, type = 7L)), 200L)
  } else as.numeric(grid)
  if (any(g <= lo) || any(g >= hi)) {
    stop("the grid must lie strictly inside the support.", call. = FALSE)
  }
  gz <- tr$g_inv(g)
  g_prime <- tr$dg(gz)
  jac <- 1 / g_prime
  dens <- rowSums(.fz_K(outer(gz, z, "-") / hh)) * jac / (n * hh)
  list(grid = g, density = dens, bandwidth = hh, transform = tr$name,
       jacobian = jac, g_prime = g_prime,
       mass = .fz_trapz(dens, g),
       boundary_bias_order = "O(h^2) everywhere, including the boundary",
       jacobian_note = paste("1/g'(g^{-1}(t)) is the change-of-variables",
                             "factor; without it the result is a density on",
                             "the transformed scale, not the original one"),
       n = n,
       method = paste("Boundary-free KDE by bijection (Ch. 4); no boundary",
                      "exists on the transformed scale"))
}


.fz_cs_common <- function(x, t_grid, h, transform) {
  xv <- .fz_check_sample(x)
  tr <- .fz_transform(transform)
  lo <- tr$support[1L]
  hi <- tr$support[2L]
  if (any(xv <= lo) || any(xv >= hi)) {
    stop("the sample must lie strictly inside the support.", call. = FALSE)
  }
  tg <- as.numeric(t_grid)
  if (any(tg <= lo) || any(tg >= hi)) {
    stop("t_grid must lie strictly inside the support.", call. = FALSE)
  }
  zx <- tr$g_inv(xv)
  hh <- .fz_check_h(if (is.null(h)) .fz_kdfe_h(zx) else as.numeric(h))
  list(tr = tr, tg = tg, zx = zx, zt = tr$g_inv(tg), hh = hh, n = length(xv))
}


#' First cumulative survival estimator (4.8)
#'
#' `V_{1,h}(x, y) = int_x^Inf g'(z) V((z - y)/h) dz`. This variant is
#' built so that `d/dt` of the cumulative survival is exactly minus
#' the survival estimator -- the structural identity of the
#' population quantities is preserved by the estimators.
#' [morie_fauzi_cumulative_survival_2()] is the mirror construction
#' and does not preserve it; the two differ in bias coefficient
#' (`b_2` here, `b_3` there) and share the same covariance.
#'
#' @param x numeric sample strictly inside the support.
#' @param t_grid evaluation points strictly inside the support.
#' @param h bandwidth on the transformed scale.
#' @param transform `"log"` or `"identity"`.
#' @return list: t_grid, S_cumulative, S_survival, bandwidth,
#'   preserves_derivative_relation (`TRUE`), bias_coefficient,
#'   mirror_note, n, method.
#' @references Fauzi and Maesono (2023), Eq. (4.8), (4.9), (4.15).
#' @examples
#' morie_fauzi_cumulative_survival_1(stats::rexp(100), c(0.5, 1))$S_survival
#' @export
morie_fauzi_cumulative_survival_1 <- function(x, t_grid, h = NULL,
                                              transform = "log") {
  cm <- .fz_cs_common(x, t_grid, h, transform)
  upper <- max(cm$zx) + 6 * cm$hh
  s_cum <- vapply(cm$zt, function(zv) {
    zz <- .fz_seq(zv, upper, 400L)
    .fz_trapz(cm$tr$dg(zz) * rowMeans(.fz_V(outer(zz, cm$zx, "-") / cm$hh)), zz)
  }, numeric(1))
  s_surv <- rowMeans(.fz_V(outer(cm$zt, cm$zx, "-") / cm$hh))
  list(t_grid = cm$tg, S_cumulative = s_cum, S_survival = s_surv,
       bandwidth = cm$hh, preserves_derivative_relation = TRUE,
       bias_coefficient = "b_2 (4.15)",
       mirror_note = paste("V_1 integrates x to infinity with argument",
                           "(z - y)/h; V_2 integrates minus infinity to y",
                           "with (x - z)/h"),
       n = cm$n,
       method = paste("First cumulative survival estimator (4.8);",
                      "d/dt gives -S_tilde exactly"))
}


#' Second cumulative survival estimator (4.17)
#'
#' The mirror of (4.8): `V_{2,h}(x, y) = int_{-Inf}^y g'(z) V((x - z)/h) dz`.
#' Multiplying `V` by `g'` is what makes this an estimator of the
#' cumulative survival function at all. It does NOT preserve the
#' derivative relation that [morie_fauzi_cumulative_survival_1()]
#' does, and its bias carries `b_3` rather than `b_2`; the covariance
#' is the same.
#'
#' @inheritParams morie_fauzi_cumulative_survival_1
#' @return list: t_grid, S_cumulative, S_survival, bandwidth,
#'   preserves_derivative_relation (`FALSE`), bias_coefficient,
#'   g_prime_note, same_covariance_as_first, n, method.
#' @references Fauzi and Maesono (2023), Eq. (4.17), (4.18), (4.21).
#' @examples
#' morie_fauzi_cumulative_survival_2(stats::rexp(60), c(0.5, 1))$S_cumulative
#' @export
morie_fauzi_cumulative_survival_2 <- function(x, t_grid, h = NULL,
                                              transform = "log") {
  cm <- .fz_cs_common(x, t_grid, h, transform)
  lower <- min(cm$zx) - 6 * cm$hh
  s_cum <- vapply(cm$zt, function(zv) {
    mean(vapply(cm$zx, function(yy) {
      zz <- .fz_seq(lower, yy, 200L)
      .fz_trapz(cm$tr$dg(zz) * .fz_V((zv - zz) / cm$hh), zz)
    }, numeric(1)))
  }, numeric(1))
  s_surv <- rowMeans(.fz_V(outer(cm$zt, cm$zx, "-") / cm$hh))
  list(t_grid = cm$tg, S_cumulative = s_cum, S_survival = s_surv,
       bandwidth = cm$hh, preserves_derivative_relation = FALSE,
       bias_coefficient = "b_3 (4.21)",
       g_prime_note = paste("multiplying V by g' is what makes this an",
                            "estimator of the cumulative survival function",
                            "at all"),
       same_covariance_as_first = TRUE,
       n = cm$n,
       method = paste("Second cumulative survival estimator (4.17);",
                      "mirror of the first, bias b_3"))
}


.fz_bias_common <- function(t, f_X, transform) {
  tv <- as.numeric(t)
  fx <- as.numeric(f_X)
  if (length(fx) != length(tv)) {
    stop(sprintf("f_X has %d entries for %d points.", length(fx), length(tv)),
         call. = FALSE)
  }
  if (any(fx < 0)) stop("a density must be non-negative.", call. = FALSE)
  tr <- .fz_transform(transform)
  if (any(tv <= tr$support[1L]) || any(tv >= tr$support[2L])) {
    stop("t must lie strictly inside the support.", call. = FALSE)
  }
  zt <- tr$g_inv(tv)
  list(tv = tv, fx = fx, tr = tr, zt = zt,
       gp = tr$dg(zt), gpp = tr$d2g(zt))
}

.fz_bias_payload <- function(cm) {
  list(g_prime = cm$gp, g_double_prime = cm$gpp,
       bias_order = "O(h^2) everywhere, including the boundary region",
       contrast = paste("the naive kernel estimator degrades to O(h) or",
                        "O(1) at the boundary (Remark 4.5)"),
       transform = cm$tr$name)
}


#' Bias coefficient b_1 (4.14)
#'
#' `b_1(t) = g''(g^{-1}(t)) f_X(t) + g'(g^{-1}(t))^2 f_X'(t)`. The
#' transformation does not remove the bias of a boundary-free
#' estimator; it makes the constant computable and keeps the order at
#' `O(h^2)` right up to the edge.
#'
#' @param t evaluation points strictly inside the support.
#' @param f_X density at `t`.
#' @param f_X_prime density derivative at `t`; required here.
#' @param S_X survival at `t`; unused by `b_1`, kept for signature
#'   parity with the `b_2` and `b_3` companions.
#' @param transform `"log"` or `"identity"`.
#' @return list: t, b_1, g_prime, g_double_prime, bias_order,
#'   contrast, transform, method.
#' @references Fauzi and Maesono (2023), Eq. (4.14).
#' @examples
#' morie_fauzi_b1_coefficient(1, f_X = 0.5, f_X_prime = -0.5)$b_1
#' @export
morie_fauzi_b1_coefficient <- function(t, f_X, f_X_prime = NULL,
                                       S_X = NULL, transform = "log") {
  cm <- .fz_bias_common(t, f_X, transform)
  if (is.null(f_X_prime)) {
    stop("b_1 needs the density derivative f_X_prime.", call. = FALSE)
  }
  fp <- as.numeric(f_X_prime)
  if (length(fp) != length(cm$tv)) {
    stop(sprintf("f_X_prime has %d entries for %d.", length(fp),
                 length(cm$tv)), call. = FALSE)
  }
  c(list(t = cm$tv, b_1 = cm$gpp * cm$fx + cm$gp^2 * fp),
    .fz_bias_payload(cm),
    list(method = paste("b_1 from Eq. (4.14); the transformation makes the",
                        "bias constant computable")))
}


#' Bias coefficient b_2 (4.15)
#'
#' `b_2(t) = g'(g^{-1}(t))^2 f_X(t) + int_{g^{-1}(t)}^Inf g''(z) g'(z) f_X(g(z)) dz`.
#' The tail integral is taken by quadrature on the transformed scale,
#' where the integrand is smooth. `b_2` is the coefficient carried by
#' the FIRST cumulative survival estimator.
#'
#' @inheritParams morie_fauzi_b1_coefficient
#' @return list: t, b_2, g_prime, g_double_prime, bias_order,
#'   contrast, transform, method.
#' @references Fauzi and Maesono (2023), Eq. (4.15).
#' @examples
#' morie_fauzi_b2_coefficient(c(1, 2), f_X = c(0.4, 0.1))$b_2
#' @export
morie_fauzi_b2_coefficient <- function(t, f_X, f_X_prime = NULL,
                                       S_X = NULL, transform = "log") {
  cm <- .fz_bias_common(t, f_X, transform)
  b2 <- vapply(seq_along(cm$zt), function(i) {
    zz <- .fz_seq(cm$zt[i], cm$zt[i] + 12, 400L)
    fv <- stats::approx(cm$tv, cm$fx, xout = cm$tr$g(zz), rule = 1L)$y
    fv[is.na(fv)] <- 0
    cm$gp[i]^2 * cm$fx[i] +
      .fz_trapz(cm$tr$d2g(zz) * cm$tr$dg(zz) * fv, zz)
  }, numeric(1))
  c(list(t = cm$tv, b_2 = b2), .fz_bias_payload(cm),
    list(method = paste("b_2 from Eq. (4.15); the transformation makes the",
                        "bias constant computable")))
}


#' Bias coefficient b_3 (4.21)
#'
#' `b_3(t) = g'(g^{-1}(t))^2 f_X(t) - g''(g^{-1}(t)) S_X(t)`, the
#' coefficient carried by the SECOND cumulative survival estimator.
#' Note the minus sign and the survival function: this is where the
#' two Ch. 4 estimators part company.
#'
#' @inheritParams morie_fauzi_b1_coefficient
#' @return list: t, b_3, g_prime, g_double_prime, bias_order,
#'   contrast, transform, method.
#' @references Fauzi and Maesono (2023), Eq. (4.21).
#' @examples
#' morie_fauzi_b3_coefficient(1, f_X = 0.5, S_X = 0.4)$b_3
#' @export
morie_fauzi_b3_coefficient <- function(t, f_X, f_X_prime = NULL,
                                       S_X = NULL, transform = "log") {
  cm <- .fz_bias_common(t, f_X, transform)
  if (is.null(S_X)) {
    stop("b_3 needs the survival function S_X.", call. = FALSE)
  }
  sx <- as.numeric(S_X)
  if (length(sx) != length(cm$tv)) {
    stop(sprintf("S_X has %d entries for %d.", length(sx), length(cm$tv)),
         call. = FALSE)
  }
  if (any(sx < 0 | sx > 1)) {
    stop("S_X must lie in [0, 1].", call. = FALSE)
  }
  c(list(t = cm$tv, b_3 = cm$gp^2 * cm$fx - cm$gpp * sx),
    .fz_bias_payload(cm),
    list(method = paste("b_3 from Eq. (4.21); the transformation makes the",
                        "bias constant computable")))
}


#' Naive kernel mean-residual-life estimator (4.2)
#'
#' The baseline whose boundary failure Ch. 4 exists to fix: bias is
#' `O(h)` near the edge of the support and can degrade to `O(1)`.
#' Compare with [morie_fauzi_mrl_boundary_free_2()] on the same data.
#'
#' @param x numeric sample.
#' @param t_grid evaluation points.
#' @param h bandwidth; `4^(1/3) sigma n^{-1/3}` if `NULL` -- the
#'   mean residual life is distribution-function-type, not
#'   density-type (its Theorem 4.3 variance is `O(1/n) - O(h/n)`).
#' @return list: t_grid, mrl, bandwidth, interior_bias_order,
#'   boundary_bias_order, boundary_safe (`FALSE`), n, method.
#' @references Fauzi and Maesono (2023), Eq. (4.2).
#' @examples
#' morie_fauzi_mrl_naive(stats::rexp(200), c(0.5, 1))$mrl
#' @export
morie_fauzi_mrl_naive <- function(x, t_grid, h = NULL) {
  xv <- .fz_check_sample(x)
  n <- length(xv)
  tg <- as.numeric(t_grid)
  hh <- .fz_check_h(if (is.null(h)) .fz_kdfe_h(xv) else as.numeric(h))
  upper <- max(xv) + 8 * hh
  mrl <- vapply(tg, function(t) {
    den <- sum(.fz_V((t - xv) / hh))
    if (den <= 0) return(NA_real_)
    zz <- .fz_seq(t, upper, 400L)
    num <- .fz_trapz(rowSums(.fz_V(outer(zz, xv, "-") / hh)), zz)
    num / den
  }, numeric(1))
  list(t_grid = tg, mrl = mrl, bandwidth = hh,
       interior_bias_order = "O(h^2)",
       boundary_bias_order = "O(h), and can degrade to O(1)",
       boundary_safe = FALSE, n = n,
       method = paste("Naive kernel MRL (4.2); the baseline whose boundary",
                      "failure Ch. 4 fixes"))
}


#' Boundary-free mean-residual-life estimator (4.24)
#'
#' `m~_{X,2}(t)` -- the ratio of the second cumulative survival
#' estimator to the survival estimator. Bias is `O(h^2)` everywhere
#' including the boundary, with the Theorem 4.3 constant
#' `h^2/(2 S_X(t)) [b_3(t) + m_X(t) b_1(t)] int y^2 K(y) dy`.
#'
#' Prefer the variant built on
#' [morie_fauzi_cumulative_survival_1()] when the analytic relation
#' between `S` and the cumulative survival must be preserved
#' (Remark 4.2).
#'
#' @param x numeric sample strictly inside the support.
#' @param t_grid evaluation points strictly inside the support.
#' @param h bandwidth on the transformed scale.
#' @param transform `"log"` or `"identity"`.
#' @return list: t_grid, mrl, bandwidth, bias_order, bias_formula,
#'   variance_vanishes_at_boundary, prefer_variant_1_when, n, method.
#' @references Fauzi and Maesono (2023), Eq. (4.24), Theorem 4.3.
#'   Transcribed from the PDF, where (4.24) is printed in full.
#' @examples
#' morie_fauzi_mrl_boundary_free_2(stats::rexp(60), c(0.5, 1))$mrl
#' @export
morie_fauzi_mrl_boundary_free_2 <- function(x, t_grid, h = NULL,
                                            transform = "log") {
  out <- morie_fauzi_cumulative_survival_2(x, t_grid, h = h,
                                           transform = transform)
  s <- out$S_survival
  mrl <- ifelse(s > 0, out$S_cumulative / pmax(s, 1e-300), NA_real_)
  list(t_grid = out$t_grid, mrl = mrl, bandwidth = out$bandwidth,
       bias_order = "O(h^2) everywhere, including the boundary region",
       bias_formula = paste("h^2/(2 S_X(t)) [b_3(t) + m_X(t) b_1(t)]",
                            "int y^2 K(y) dy"),
       variance_vanishes_at_boundary = TRUE,
       prefer_variant_1_when = paste("the analytic relation between S and",
                                     "S_cum must be preserved (Remark 4.2)"),
       n = out$n,
       method = paste("Boundary-free MRL estimator m_tilde_{X,2} (4.24);",
                      "Theorem 4.3 bias"))
}


#' Theorem 4.3: biases and variances of the boundary-free MRL estimators
#'
#' (4.25)-(4.28). The two estimators' biases are identical except
#' that the first carries `b_2` and the second `b_3`; everything else
#' is shared (Remark 4.3). The variance is `O(1/n)` with the
#' bandwidth entering only at `O(h/n)`, so it is far less
#' bandwidth-sensitive than the bias.
#'
#' @param t evaluation points.
#' @param S_X survival at `t`, strictly positive.
#' @param S_bar_X the cumulative survival at `t`.
#' @param m_X mean residual life at `t`.
#' @param b1 coefficient `b_1` at `t`.
#' @param b2,b3 coefficients; supply either or both.
#' @param n sample size.
#' @param h bandwidth.
#' @param g_prime,f_X transformation derivative and density at `t`.
#' @param mu2 `int y^2 K(y) dy`.
#' @param VW `int V(y) W(y) dy`; the Gaussian value if `NULL`.
#' @return list: t, bias_1, bias_2, variance, b4, b5,
#'   differ_only_in, variance_leading_order, n, h, method.
#' @references Fauzi and Maesono (2023), Theorem 4.3.
#' @examples
#' morie_fauzi_theorem_4_3(1, 0.4, 0.5, 1.25, 0.3, b2 = 0.7)$bias_1
#' @export
morie_fauzi_theorem_4_3 <- function(t, S_X, S_bar_X, m_X, b1,
                                    b2 = NULL, b3 = NULL, n = 100,
                                    h = 0.1, g_prime = 1, f_X = 1,
                                    mu2 = 1, VW = NULL) {
  tv <- as.numeric(t)
  s <- as.numeric(S_X)
  sb <- as.numeric(S_bar_X)
  m <- as.numeric(m_X)
  c1 <- as.numeric(b1)
  for (nm in c("S_X", "S_bar_X", "m_X", "b1")) {
    arr <- switch(nm, S_X = s, S_bar_X = sb, m_X = m, b1 = c1)
    if (length(arr) != length(tv)) {
      stop(sprintf("%s has %d entries for %d.", nm, length(arr), length(tv)),
           call. = FALSE)
    }
  }
  if (any(s <= 0)) {
    stop("S_X must be strictly positive to divide by it.", call. = FALSE)
  }
  nn <- as.integer(n)
  hh <- as.numeric(h)
  if (is.na(nn) || nn < 2L || hh <= 0) {
    stop(sprintf("need n >= 2 and h > 0, got (%s, %g).", format(n), hh),
         call. = FALSE)
  }
  vw <- if (is.null(VW)) 1 / (2 * sqrt(pi)) else as.numeric(VW)
  gp <- rep(as.numeric(g_prime), length.out = length(tv))
  fx <- rep(as.numeric(f_X), length.out = length(tv))
  b4 <- 2 * sb - s * m^2
  b5 <- gp * fx * m^2
  bias1 <- if (is.null(b2)) NULL else
    hh^2 / (2 * s) * (as.numeric(b2) + m * c1) * as.numeric(mu2)
  bias2 <- if (is.null(b3)) NULL else
    hh^2 / (2 * s) * (as.numeric(b3) + m * c1) * as.numeric(mu2)
  list(t = tv, bias_1 = bias1, bias_2 = bias2,
       variance = b4 / (nn * s^2) - hh * b5 / (nn * s^2) * vw,
       b4 = b4, b5 = b5,
       differ_only_in = paste("b_2 for the first estimator, b_3 for the",
                              "second; everything else is shared",
                              "(Remark 4.3)"),
       variance_leading_order = paste("O(1/n); the bandwidth enters only at",
                                      "O(h/n), so the variance is far less",
                                      "bandwidth-sensitive than the bias"),
       n = nn, h = hh,
       method = paste("Theorem 4.3 (4.25)-(4.28): biases and variances of",
                      "the boundary-free MRL estimators"))
}


#' Theorem 4.4: asymptotic normality of the boundary-free MRL estimators
#'
#' The standardised estimators are asymptotically `N(0, 1)`, and the
#' statement holds AT the boundary as well as inside. The Lyapunov
#' condition needs no extra assumption because `V` is bounded in
#' `[0, 1]`, so every moment exists automatically.
#'
#' @param mrl_hat estimated mean residual life.
#' @param mrl_true the value under the null.
#' @param variance the Theorem 4.3 variance.
#' @return list: z, p_two_sided, holds_for, why_lyapunov_works,
#'   valid_at_boundary, method.
#' @references Fauzi and Maesono (2023), Theorem 4.4.
#' @examples
#' morie_fauzi_theorem_4_4(1.2, 1, 0.01)$z
#' @export
morie_fauzi_theorem_4_4 <- function(mrl_hat, mrl_true, variance) {
  mh <- as.numeric(mrl_hat)
  mt <- as.numeric(mrl_true)
  v <- as.numeric(variance)
  if (length(mh) != length(mt) || length(mh) != length(v)) {
    stop("all three arguments must have the same length.", call. = FALSE)
  }
  if (any(v < 0)) stop("variances must be non-negative.", call. = FALSE)
  sd_v <- sqrt(pmax(v, 0))
  z <- ifelse(sd_v > 0, (mh - mt) / pmax(sd_v, 1e-300), NA_real_)
  list(z = z, p_two_sided = 2 * stats::pnorm(abs(z), lower.tail = FALSE),
       holds_for = "both m_tilde_{X,1} and m_tilde_{X,2}",
       why_lyapunov_works = paste("V is bounded in [0, 1], so every moment",
                                  "exists automatically and the Lyapunov",
                                  "condition needs no extra assumption"),
       valid_at_boundary = TRUE,
       method = paste("Theorem 4.4: standardised boundary-free MRL",
                      "estimators are asymptotically N(0, 1)"))
}


#' Theorem 4.5: strong uniform consistency on a bounded interval
#'
#' Stronger than pointwise convergence, and what licenses using the
#' whole estimated curve -- a maximum, a crossing point -- rather
#' than one pre-chosen `t`. The interval must be BOUNDED; the proof
#' runs by monotonicity plus pointwise convergence on a finite grid,
#' as in Glivenko-Cantelli.
#'
#' @param mrl_hat estimated curve.
#' @param mrl_true true curve, same length.
#' @param t_grid the grid both are evaluated on.
#' @param interval optional bounded `c(lo, hi)` to restrict to.
#' @return list: sup_error, argmax_t, interval, mode,
#'   requires_bounded_B, stronger_than_pointwise, proof_device,
#'   licenses, method.
#' @references Fauzi and Maesono (2023), Theorem 4.5.
#' @examples
#' tg <- seq(0, 2, length.out = 21)
#' morie_fauzi_theorem_4_5(rep(1.1, 21), rep(1, 21), tg)$sup_error
#' @export
morie_fauzi_theorem_4_5 <- function(mrl_hat, mrl_true, t_grid,
                                    interval = NULL) {
  mh <- as.numeric(mrl_hat)
  mt <- as.numeric(mrl_true)
  tg <- as.numeric(t_grid)
  if (length(mh) != length(mt) || length(mh) != length(tg)) {
    stop("all three arguments must have the same length.", call. = FALSE)
  }
  if (is.null(interval)) {
    sel <- rep(TRUE, length(tg))
    iv <- c(min(tg), max(tg))
  } else {
    lo <- as.numeric(interval[1L])
    hi <- as.numeric(interval[2L])
    if (!is.finite(lo) || !is.finite(hi) || hi <= lo) {
      stop(paste("the interval must be bounded with lo < hi; uniform",
                 "consistency is stated on a BOUNDED B."), call. = FALSE)
    }
    sel <- tg >= lo & tg <= hi
    iv <- c(lo, hi)
  }
  if (!any(sel)) {
    stop("no grid points fall inside the interval.", call. = FALSE)
  }
  err <- abs(mh[sel] - mt[sel])
  k <- which.max(err)
  list(sup_error = max(err, na.rm = TRUE), argmax_t = tg[sel][k],
       interval = iv, mode = "uniform, almost sure",
       requires_bounded_B = TRUE, stronger_than_pointwise = TRUE,
       proof_device = paste("monotonicity plus pointwise convergence on a",
                            "finite grid, as in Glivenko-Cantelli"),
       licenses = paste("using the whole estimated curve -- a maximum, a",
                        "crossing point -- not just one pre-chosen t"),
       method = paste("Theorem 4.5: strong uniform consistency on a",
                      "bounded interval"))
}


#' Theorem 4.6: the mean-residual-life identity at the start of support
#'
#' (4.29): `m~(a_1) + a_1 = Xbar + O_p(h^2)`. At the left end of the
#' support nobody has failed yet, so the expected remaining lifetime
#' is the expected lifetime. A large gap is a diagnostic: a bandwidth
#' too big, or a transformation mismatched to the support.
#'
#' @param x numeric sample.
#' @param a1 the lower bound of the support.
#' @param mrl_at_a1 the estimated MRL at `a1`.
#' @param h bandwidth, to set the `O(h^2)` tolerance; optional.
#' @return list: identity_lhs, sample_mean, gap, expected_order,
#'   within_expected, a1, n, why_it_holds, diagnostic_use, method.
#' @references Fauzi and Maesono (2023), Theorem 4.6, Eq. (4.29).
#' @examples
#' x <- stats::rexp(200)
#' morie_fauzi_theorem_4_6(x, 0, mean(x))$gap
#' @export
morie_fauzi_theorem_4_6 <- function(x, a1, mrl_at_a1, h = NULL) {
  xv <- .fz_check_sample(x)
  a <- as.numeric(a1)
  if (any(xv < a)) {
    stop(sprintf("a1 = %g is not a lower bound of the sample.", a),
         call. = FALSE)
  }
  lhs <- as.numeric(mrl_at_a1) + a
  xbar <- mean(xv)
  gap <- abs(lhs - xbar)
  tol <- if (is.null(h)) NULL else as.numeric(h)^2
  list(identity_lhs = lhs, sample_mean = xbar, gap = gap,
       expected_order = "O(h^2)",
       within_expected = if (is.null(tol)) NULL else gap <= 5 * tol,
       a1 = a, n = length(xv),
       why_it_holds = paste("at the start of the support everyone is still",
                            "at risk, so MRL(a_1) + a_1 is the overall mean"),
       diagnostic_use = paste("a large gap indicates a bandwidth too big or",
                              "a transformation mismatched to the support"),
       method = "Theorem 4.6 (4.29): m_tilde(a_1) + a_1 = Xbar + O_p(h^2)")
}


#' Conditions C1-C6 of Chapter 4
#'
#' C5 and C6 are the ones that bind in practice: they are what make
#' the bias and variance formulas derivable, and C6 (existence of
#' `E X`, `E X^2`, `E X^3`) rules out heavy-tailed data entirely.
#'
#' @param x optional sample, to evaluate the C6 moments.
#' @param transform `"log"` or `"identity"`.
#' @param check_moments compute the empirical C6 moments.
#' @return list: conditions, C3_bijective, C6_moments,
#'   heavy_tail_warning, binding_in_practice, why, transform, method.
#' @references Fauzi and Maesono (2023), Ch. 4, conditions C1-C6.
#' @examples
#' morie_fauzi_conditions_c1_c6(stats::rexp(50))$binding_in_practice
#' @export
morie_fauzi_conditions_c1_c6 <- function(x = NULL, transform = "log",
                                         check_moments = TRUE) {
  tr <- .fz_transform(transform)
  conds <- list(
    C1 = "standard kernel condition",
    C2 = "standard kernel condition",
    C3 = "bijectivity and simplicity of the transformation g",
    C4 = "validity of the serial expansions used in the proofs",
    C5 = "int g'(ux)K(x)dx and int g'(ux)V(x)dx finite near the origin",
    C6 = "E(X), E(X^2) and E(X^3) exist")
  moments <- NULL
  heavy <- NULL
  if (!is.null(x) && isTRUE(check_moments)) {
    xv <- .fz_check_sample(x, min_n = 4L)
    moments <- list(E_X = mean(xv), E_X2 = mean(xv^2), E_X3 = mean(xv^3))
    # a crude tail check: compare the empirical third moment with what
    # a same-variance normal would give
    sd_x <- stats::sd(xv)
    ref <- moments$E_X^3 + 3 * moments$E_X * sd_x^2
    heavy <- ref > 0 && moments$E_X3 > 20 * ref
  }
  list(conditions = conds, C3_bijective = TRUE, C6_moments = moments,
       heavy_tail_warning = heavy,
       binding_in_practice = c("C5", "C6"),
       why = paste("C5 and C6 are what make the bias and variance formulas",
                   "derivable; C6 rules out heavy-tailed data entirely"),
       transform = tr$name,
       method = paste("Conditions C1-C6 of Ch. 4, with the C6 moment check",
                      "made explicit"))
}


#' Kernel quantile estimator (3.1)
#'
#' Smooths in the PROBABILITY argument, not in `x`. The sample
#' quantile uses a single order statistic and therefore jumps as `p`
#' crosses `i/n`; this averages neighbouring order statistics with
#' kernel weights, which removes the jumps and reduces variance --
#' and the tails, where a single order statistic is a poor estimate,
#' are exactly where that matters.
#'
#' The weight on the `i`-th order statistic is the kernel mass of
#' `((i-1)/n, i/n]`, computed as an exact difference of the
#' integrated kernel rather than by quadrature.
#'
#' Not to be confused with [morie_kernel_quantile()], which is
#' Horowitz's conditional quantile regression.
#'
#' @param x numeric sample.
#' @param p probability levels strictly in `(0, 1)`.
#' @param h bandwidth on the probability scale; `n^{-2/5}` if `NULL`.
#' @return list: p, quantile, sample_quantile, bandwidth,
#'   weights_sum, smooths_in, why, n, method.
#' @references Fauzi and Maesono (2023), Eq. (3.1). From the PDF.
#' @examples
#' morie_fauzi_kernel_quantile(stats::rexp(200), c(0.25, 0.5))$quantile
#' @export
morie_fauzi_kernel_quantile <- function(x, p, h = NULL) {
  xv <- sort(.fz_check_sample(x, min_n = 3L))
  n <- length(xv)
  pv <- as.numeric(p)
  if (any(pv <= 0 | pv >= 1)) {
    stop("probability levels must lie strictly in (0, 1).", call. = FALSE)
  }
  hh <- .fz_check_h(if (is.null(h)) n^(-0.4) else as.numeric(h))
  edges <- (0:n) / n
  wl <- .fz_W(outer(edges, pv, "-") / hh)
  wi <- wl[-1L, , drop = FALSE] - wl[-(n + 1L), , drop = FALSE]
  wsum <- colSums(wi)
  list(p = pv, quantile = as.numeric(colSums(wi * xv) / wsum),
       sample_quantile = unname(stats::quantile(xv, pv, type = 7L)),
       bandwidth = hh, weights_sum = wsum,
       smooths_in = "the PROBABILITY argument, not in x",
       why = paste("the sample quantile uses one order statistic and jumps",
                   "as p crosses i/n; the tails are exactly where that hurts"),
       n = n,
       method = paste("Kernel quantile estimator (3.1) as a weighted sum of",
                      "order statistics"))
}


#' AMSE of the sample quantile (3.3)
#'
#' `p(1-p)/(n f(Q(p))^2)`, equivalently `Q'(p)^2 p(1-p)/n` since
#' `Q' = 1/f(Q)`. As `p` approaches 0 or 1 the binomial part shrinks,
#' which suggests extreme quantiles are easier -- they are not, since
#' the density part grows faster for thinning tails.
#'
#' @param p probability levels strictly in `(0, 1)`.
#' @param n sample size.
#' @param f_at_quantile density at `Q(p)`; supply this or `Q_prime`.
#' @param Q_prime the quantile-function derivative at `p`.
#' @return list: p, amse, se, binomial_part, density_part, n,
#'   tail_note, method.
#' @references Fauzi and Maesono (2023), Eq. (3.3).
#' @examples
#' morie_fauzi_quantile_amse(0.5, 500, f_at_quantile = 0.5)$amse
#' @export
morie_fauzi_quantile_amse <- function(p, n, f_at_quantile = NULL,
                                      Q_prime = NULL) {
  pv <- as.numeric(p)
  if (any(pv <= 0 | pv >= 1)) {
    stop("probability levels must lie strictly in (0, 1).", call. = FALSE)
  }
  nn <- as.integer(n)
  if (is.na(nn) || nn < 2L) {
    stop(sprintf("n must be at least 2, got %s.", format(n)), call. = FALSE)
  }
  if (is.null(f_at_quantile) && is.null(Q_prime)) {
    stop(paste("supply either the density at the quantile or Q'(p);",
               "the AMSE is not determined by p and n alone."), call. = FALSE)
  }
  if (!is.null(Q_prime)) {
    qp <- as.numeric(Q_prime)
    if (length(qp) != length(pv)) {
      stop(sprintf("Q_prime has %d entries for %d.", length(qp), length(pv)),
           call. = FALSE)
    }
    qp[qp == 0] <- NA_real_
  } else {
    dens <- as.numeric(f_at_quantile)
    if (length(dens) != length(pv)) {
      stop(sprintf("f_at_quantile has %d for %d.", length(dens), length(pv)),
           call. = FALSE)
    }
    if (any(dens <= 0)) {
      stop("the density at the quantile must be positive.", call. = FALSE)
    }
    qp <- 1 / dens
  }
  binom <- pv * (1 - pv) / nn
  amse <- qp^2 * binom
  list(p = pv, amse = amse, se = sqrt(pmax(amse, 0)),
       binomial_part = binom, density_part = qp^2, n = nn,
       tail_note = paste("as p goes to 0 or 1 the binomial part shrinks but",
                         "the density part grows faster for thinning tails,",
                         "so the AMSE increases -- that is why extreme",
                         "quantiles are hard"),
       method = paste("AMSE of the sample quantile (3.3);",
                      "p(1-p)/(n f^2) = Q'(p)^2 p(1-p)/n"))
}


#' Order-m kernel and its moments
#'
#' Moments `1..m-1` vanish, pushing the bias to `O(h^m)`. A
#' non-negative function cannot have a vanishing second moment, so
#' orders above 2 necessarily take NEGATIVE values -- the density can
#' go negative and the distribution non-monotone. Acceptable for
#' quantiles, where the estimand is a location; not for a density to
#' be plotted.
#'
#' @param u evaluation points.
#' @param m kernel order: 2, 4 or 6.
#' @return list: u, K, order, moments, takes_negative_values,
#'   bias_order, tradeoff, method.
#' @references Fauzi and Maesono (2023), Ch. 3; Mueller (1991).
#' @examples
#' morie_fauzi_order_m_kernel(c(0, 1, 2), m = 4)$K
#' @export
morie_fauzi_order_m_kernel <- function(u, m = 4L) {
  uv <- as.numeric(u)
  mm <- as.integer(m)
  k <- .fz_muller(uv, mm)
  grid <- .fz_seq(-10, 10, 4001L)
  kg <- .fz_muller(grid, mm)
  moments <- stats::setNames(
    lapply(0:mm, function(j) .fz_trapz(grid^j * kg, grid)),
    as.character(0:mm))
  list(u = uv, K = k, order = mm, moments = moments,
       takes_negative_values = any(kg < -1e-12),
       bias_order = sprintf("O(h^%d)", mm),
       tradeoff = paste("a vanishing second moment forces negative values,",
                        "so the density can go negative and the",
                        "distribution non-monotone; acceptable for",
                        "quantiles, not for a density to be plotted"),
       method = sprintf(paste("Order-%d kernel; moments 1..%d vanish,",
                              "pushing the bias to O(h^%d)"),
                        mm, mm - 1L, mm))
}


#' Fourth-order Mueller kernel
#'
#' `(3 - u^2) phi(u) / 2`, which changes sign exactly at
#' `u^2 = 3` by construction and has `mu_2 = 0`, giving an `O(h^4)`
#' bias.
#'
#' @param u evaluation points.
#' @return list: u, K, mu0, mu2, mu4, negative_beyond, bias_order,
#'   method.
#' @references Fauzi and Maesono (2023), Ch. 3; Mueller (1991).
#' @examples
#' morie_fauzi_muller_kernel(c(0, 1, 2))$K
#' @export
morie_fauzi_muller_kernel <- function(u) {
  uv <- as.numeric(u)
  grid <- .fz_seq(-10, 10, 4001L)
  kg <- .fz_muller(grid, 4L)
  list(u = uv, K = .fz_muller(uv, 4L),
       mu0 = .fz_trapz(kg, grid),
       mu2 = .fz_trapz(grid^2 * kg, grid),
       mu4 = .fz_trapz(grid^4 * kg, grid),
       negative_beyond = sqrt(3),
       bias_order = "O(h^4)",
       method = paste("Fourth-order Mueller kernel (3 - u^2)phi(u)/2;",
                      "mu_2 = 0 by construction"))
}


#' Lemma 3.1: asymptotic representation of the kernel quantile estimator
#'
#' A Bahadur-type representation -- an i.i.d. average plus a
#' smaller-order remainder -- which is what makes normality, the
#' asymptotic variance and the Edgeworth expansion all follow from
#' standard theory for sums.
#'
#' The lemma expands about the POPULATION quantile. Centring on the
#' sample quantile instead makes the linear term vanish identically,
#' because the empirical df at its own `p`-quantile is `p` up to
#' `1/n`; the decomposition then carries no information and the
#' variance it licenses is unsupported. When `q_true` is not
#' supplied the sample quantile is used and `centred_at` says so.
#'
#' @param x numeric sample.
#' @param p probability level strictly in `(0, 1)`.
#' @param h bandwidth passed to the quantile estimator.
#' @param q_true the population quantile, when known.
#' @return list: influence, linear_term, estimate, remainder,
#'   density_at_quantile, centre, centred_at, asymptotic_variance,
#'   representation, n, method.
#' @references Fauzi and Maesono (2023), Lemma 3.1. From the PDF.
#' @examples
#' morie_fauzi_lemma_3_1(stats::rexp(200), 0.5, q_true = log(2))$linear_term
#' @export
morie_fauzi_lemma_3_1 <- function(x, p, h = NULL, q_true = NULL) {
  xv <- .fz_check_sample(x, min_n = 5L)
  n <- length(xv)
  pp <- as.numeric(p)
  if (!(pp > 0 && pp < 1)) {
    stop(sprintf("p must lie strictly in (0, 1), got %g.", pp), call. = FALSE)
  }
  if (is.null(q_true)) {
    centred_at <- paste("sample quantile -- the linear term is degenerate",
                        "here; supply q_true for the lemma as stated")
    q <- unname(stats::quantile(xv, pp, type = 7L))
  } else {
    centred_at <- "population quantile (supplied)"
    q <- as.numeric(q_true)
  }
  hb <- 1.06 * stats::sd(xv) * n^(-0.2)
  f_q <- mean(.fz_K((q - xv) / hb)) / hb
  if (f_q <= 0) {
    stop(paste("the estimated density at the quantile is zero;",
               "the representation divides by it."), call. = FALSE)
  }
  infl <- (pp - as.numeric(xv <= q)) / f_q
  lin <- mean(infl)
  est <- morie_fauzi_kernel_quantile(xv, pp, h = h)$quantile[1L]
  list(influence = infl, linear_term = lin, estimate = est,
       remainder = est - q - lin,
       density_at_quantile = f_q, centre = q, centred_at = centred_at,
       asymptotic_variance = pp * (1 - pp) / (n * f_q^2),
       representation = paste("Bahadur-type: an i.i.d. average plus a",
                              "smaller-order remainder, which is what makes",
                              "normality, the variance and the Edgeworth",
                              "expansion all follow from standard theory",
                              "for sums"),
       n = n,
       method = paste("Lemma 3.1: asymptotic representation of the kernel",
                      "quantile estimator"))
}
