# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Kosorok shelf mirrors -- the surface NOT already covered by
# R/ksr01.R .. R/ksr20.R.
#
# The collision scan found twenty existing morie_ksrNN_* functions
# (empirical process, Donsker class, Glivenko-Cantelli, VC dimension,
# bracketing number, maximal inequality, both bootstraps, Z- and
# M-estimators, efficient score, information bound, tangent space,
# profile likelihood, one-step estimator, influence function, counting
# process, Nelson-Aalen, Cox partial likelihood, censoring survival).
# This file adds only what those do not: the Brownian bridge
# covariance, the exact sup norm, LIL and KMT bounds, U-processes,
# entropy integrals and the Donsker conditions, the functional delta
# method with Frechet and invertibility checks, the Kaplan-Meier
# Hadamard derivative, differentiability in quadratic mean, the
# quantile Hadamard sandwich and the bounded-Lipschitz metric.
#
# Mirrors morie.fn._kosorok and the ksr0xx modules. PDF-verified:
# bridge covariance F(s ^ t) - F(s)F(t); LIL eq. (2.21) bound 1/2;
# Chung liminf pi/2.

#' Brownian bridge covariance
#'
#' \eqn{cov\[G(s), G(t)\] = F(s \wedge t) - F(s)F(t)} (Kosorok Ch. 2).
#' Mirrors \code{morie.fn.ksr030}.
#'
#' @param s,t numeric time points.
#' @param F optional CDF; the uniform on \[0, 1\] when NULL.
#' @return list: covariance, variance_s.
#' @references Kosorok, M. R. (2008). Introduction to Empirical
#'   Processes and Semiparametric Inference. Springer. Ch. 2.
#' @examples
#' morie_bridge_covariance(0.3, 0.7)$covariance
#' @export
morie_bridge_covariance <- function(s, t, F = NULL) {
  s <- as.numeric(s)
  t <- as.numeric(t)
  cdf <- function(z) if (is.null(F)) pmin(pmax(z, 0), 1) else vapply(z, F, 0)
  cv <- cdf(pmin(s, t)) - cdf(s) * cdf(t)
  list(covariance = cv, variance_s = cdf(pmin(s, s)) - cdf(s)^2,
       s = s, t = t,
       method = "cov[G(s), G(t)] = F(s ^ t) - F(s)F(t) (Kosorok Ch. 2)")
}

#' Exact uniform norm of the empirical process
#'
#' \eqn{\|G_n\|_\infty} evaluated at the order statistics, where the
#' supremum of a step-minus-continuous process is always attained. A
#' fixed grid understates it. Mirrors \code{morie.fn._kosorok.sup_norm}.
#'
#' @param x numeric sample.
#' @param F optional CDF; uniform when NULL.
#' @return numeric sup norm.
#' @references Kosorok (2008), Ch. 2.
#' @examples
#' morie_empirical_sup_norm(runif(50))
#' @export
morie_empirical_sup_norm <- function(x, F = NULL) {
  xs <- sort(as.numeric(x))
  n <- length(xs)
  if (n < 1L) stop("x must be non-empty.", call. = FALSE)
  Ft <- if (is.null(F)) pmin(pmax(xs, 0), 1) else vapply(xs, F, 0)
  sqrt(n) * max(max(seq_len(n) / n - Ft), max(Ft - (seq_len(n) - 1) / n))
}

#' Law of the iterated logarithm ratio and the Chung liminf constant
#'
#' Kosorok eq. (2.21): \eqn{\limsup \|G_n\|_\infty /
#' \sqrt{2\log\log n} \le 1/2} a.s., with Chung's companion
#' \eqn{\liminf \sqrt{2\log\log n}\|G_n\|_\infty = \pi/2}. Mirrors
#' \code{morie.fn.ksr058}.
#'
#' @param x numeric sample.
#' @param F optional CDF.
#' @return list: sup_norm, lil_ratio, lil_bound (0.5),
#'   chung_liminf_constant (pi/2), loglog_term, n.
#' @references Kosorok (2008), eq. (2.21).
#' @examples
#' morie_lil_ratio(runif(1000))$lil_ratio
#' @export
morie_lil_ratio <- function(x, F = NULL) {
  x <- as.numeric(x)
  n <- length(x)
  if (n < 16L) stop("need at least 16 observations.", call. = FALSE)
  ll <- log(log(n))
  if (ll <= 0) stop("n too small for log log n.", call. = FALSE)
  s <- morie_empirical_sup_norm(x, F)
  den <- sqrt(2 * ll)
  list(sup_norm = s, lil_ratio = s / den, lil_bound = 0.5,
       chung_liminf_constant = pi / 2, loglog_term = den, n = n,
       method = "||G_n||_inf / sqrt(2 log log n) vs the 1/2 bound (eq. 2.21)")
}

#' KMT strong-approximation bound
#'
#' \eqn{P(\|G_n - B_n(F)\|_\infty > (a\log n + x)/\sqrt n) \le
#' b e^{-cx}}. The universal constants a, b, c are stated NOWHERE in
#' the original KMT papers or in Kosorok, so they must be supplied;
#' Ye & Austern (2025) derive computable substitutes. Mirrors
#' \code{morie.fn.ksr059}.
#'
#' @param n sample size.
#' @param x deviation parameter.
#' @param a,b,c the universal constants; all three required.
#' @return list: threshold, probability_bound, n.
#' @references Komlos, Major & Tusnady (1975), Z. Wahrsch. Verw.
#'   Gebiete 32(1-2), 111-131. Ye, H. & Austern, M. (2025),
#'   arXiv:2508.03833.
#' @examples
#' morie_kmt_bound(1000, x = 2, a = 1, b = 1, c = 1)$threshold
#' @export
morie_kmt_bound <- function(n, x = 1, a = NULL, b = NULL, c = NULL) {
  n <- as.integer(n)
  if (n < 2L) stop("n must be at least 2.", call. = FALSE)
  if (is.null(a) || is.null(b) || is.null(c)) {
    stop(paste("the KMT constants a, b and c are universal but their values",
               "are not given in the literature; supply them explicitly."),
         call. = FALSE)
  }
  thr <- (a * log(n) + x) / sqrt(n)
  list(threshold = thr, probability_bound = min(1, b * exp(-c * x)), n = n,
       method = "KMT: ||G_n - B_n||_inf <= (a log n + x)/sqrt(n) w.h.p.")
}

#' U-statistic of order m
#'
#' \eqn{U_{n,m}(f) = \binom{n}{m}^{-1} \sum_{i_1<\dots<i_m}
#' f(X_{i_1},\dots,X_{i_m})}, with the Hajek projection variance
#' \eqn{m^2\zeta_1/n}. Summands are DEPENDENT, which is why U-processes
#' need their own maximal inequalities. Mirrors
#' \code{morie.fn.ksr060}.
#'
#' @param f kernel of m arguments.
#' @param x numeric sample.
#' @param m kernel order (2 supported exactly).
#' @return list: U, n_subsets, zeta1, hajek_var, m, n.
#' @references Kosorok (2008), Ch. 2.
#' @examples
#' morie_u_process(function(a, b) abs(a - b) / 2, runif(30))$U
#' @export
morie_u_process <- function(f, x, m = 2L) {
  x <- as.numeric(x)
  n <- length(x)
  m <- as.integer(m)
  if (m != 2L) stop("only m = 2 is supported exactly.", call. = FALSE)
  if (n < 2L) stop("need at least 2 observations.", call. = FALSE)
  if (choose(n, m) > 200000) {
    stop("too many subsets to enumerate exactly; subsample first.", call. = FALSE)
  }
  idx <- utils::combn(n, 2L)
  vals <- vapply(seq_len(ncol(idx)),
                 function(k) f(x[idx[1, k]], x[idx[2, k]]), 0)
  U <- mean(vals)
  g <- numeric(n)
  cnt <- numeric(n)
  for (k in seq_len(ncol(idx))) {
    for (i in idx[, k]) {
      g[i] <- g[i] + vals[k]
      cnt[i] <- cnt[i] + 1
    }
  }
  g <- ifelse(cnt > 0, g / pmax(cnt, 1), U)
  z1 <- if (n > 1L) stats::var(g) else 0
  list(U = U, n_subsets = ncol(idx), zeta1 = z1,
       hajek_var = m^2 * z1 / n, m = m, n = n,
       method = "U_{n,m}(f) over all m-subsets, with the Hajek variance")
}

#' Bracketing entropy integral and the Donsker conditions
#'
#' \eqn{J_{\[\]}(\delta) = \int_0^\delta \sqrt{\log N_{\[\]}(\epsilon)}
#' d\epsilon}. The square root is what makes polynomial bracketing
#' growth integrable and exponential growth divergent -- that IS the
#' Donsker boundary. Also reports the two envelope conditions, which
#' differ between GC (needs P*F) and Donsker (needs P*F^2). Mirrors
#' \code{morie.fn.ksr035}--\code{ksr038}.
#'
#' @param N_bracket function mapping eps to a bracketing number.
#' @param delta upper limit.
#' @param envelope_mean optional P*F, for the GC condition.
#' @param envelope_sq_mean optional P*F^2, for the Donsker condition.
#' @return list: J, finite, gc_conditions_met, donsker_conditions_met.
#' @references Kosorok (2008), Ch. 2.
#' @examples
#' morie_entropy_integral(function(e) (1 / e)^3)$J
#' @export
morie_entropy_integral <- function(N_bracket, delta = 1, envelope_mean = NULL,
                                   envelope_sq_mean = NULL) {
  delta <- as.numeric(delta)
  if (delta <= 0) stop("delta must be positive.", call. = FALSE)
  integrand <- function(e) {
    vapply(e, function(ei) {
      nn <- N_bracket(ei)
      if (!is.finite(nn) || nn < 1) 0 else sqrt(log(nn))
    }, 0)
  }
  val <- tryCatch(
    stats::integrate(integrand, 1e-10, delta, rel.tol = 1e-8,
                     stop.on.error = FALSE)$value,
    error = function(e) Inf
  )
  fin <- is.finite(val) && val < 1e6
  list(J = val, finite = fin,
       gc_conditions_met = if (is.null(envelope_mean)) NA else
         fin && is.finite(envelope_mean),
       donsker_conditions_met = if (is.null(envelope_sq_mean)) NA else
         fin && is.finite(envelope_sq_mean),
       delta = delta,
       method = "J_[](delta) = int sqrt(log N_[](eps)) deps; GC needs P*F, Donsker P*F^2")
}

#' Functional delta method with its remainder
#'
#' \eqn{r_n(\phi(X_n) - \phi(\theta)) \Rightarrow \phi'_\theta(X)}.
#' The remainder is RETURNED rather than assumed to be o_P(1), so a
#' non-differentiable phi is visible. Mirrors
#' \code{morie.fn.ksr042}.
#'
#' @param phi functional.
#' @param X_n statistic value.
#' @param theta centring value.
#' @param r_n scaling rate.
#' @return list: scaled_increment, linear_approximation, remainder,
#'   derivative.
#' @references Kosorok (2008), Ch. 2.
#' @examples
#' morie_functional_delta(function(z) z^2, 2.01, 2, 100)$remainder
#' @export
morie_functional_delta <- function(phi, X_n, theta, r_n) {
  r_n <- as.numeric(r_n)
  if (r_n <= 0) stop("r_n must be positive.", call. = FALSE)
  Xn <- as.numeric(X_n)
  th <- as.numeric(theta)
  dev <- Xn - th
  # frechet_check returns the JACOBIAN; the delta method needs it
  # APPLIED to the observed deviation, matching morie.fn.ksr042 which
  # returns the directional derivative. Using the raw Jacobian here
  # inflates the linear term by 1/dev and the remainder is nonsense.
  jac <- morie_frechet_check(phi, th, list(dev))$derivative
  der <- jac * dev
  actual <- r_n * (as.numeric(phi(Xn)) - as.numeric(phi(th)))
  linear <- r_n * der
  list(scaled_increment = actual, linear_approximation = linear,
       remainder = actual - linear, derivative = der, jacobian = jac,
       r_n = r_n,
       method = "r_n(phi(X_n) - phi(theta)) vs phi'; remainder shown, not assumed")
}

#' Frechet differentiability check
#'
#' \eqn{\|\phi(\theta+h) - \phi(\theta) - \phi'_\theta(h)\|/\|h\|
#' \to 0}. Frechet needs ONE linear map valid in every direction, so
#' the numerical derivative is a single Jacobian -- recomputing a
#' directional derivative per h would return the Hadamard derivative
#' and report even a kinked map as Frechet differentiable. Mirrors
#' \code{morie.fn.ksr050}.
#'
#' @param phi functional.
#' @param theta base point.
#' @param h_n list of shrinking perturbations.
#' @return list: ratios, norms, ratio_shrinking, derivative.
#' @references Kosorok (2008), Ch. 2. van der Vaart (1998),
#'   Asymptotic Statistics, Lemma 21.3 (the quantile map is Hadamard
#'   but not Frechet). Reeds, J. A. (1976), PhD thesis, Harvard.
#' @examples
#' morie_frechet_check(function(z) z^2, 1, list(0.1, 0.05, 0.01))$ratio_shrinking
#' @export
morie_frechet_check <- function(phi, theta, h_n) {
  th <- as.numeric(theta)
  base <- as.numeric(phi(th))
  tt <- 1e-6
  jac <- (as.numeric(phi(th + tt)) - as.numeric(phi(th - tt))) / (2 * tt)
  seq_h <- lapply(h_n, as.numeric)
  norms <- vapply(seq_h, function(h) sqrt(sum(h^2)), 0)
  if (any(norms <= 0)) stop("perturbations must be non-zero.", call. = FALSE)
  ratios <- vapply(seq_along(seq_h), function(i) {
    h <- seq_h[[i]]
    abs(as.numeric(phi(th + h)) - base - jac * h) / norms[i]
  }, 0)
  ord <- order(-norms)
  list(ratios = ratios, norms = norms, derivative = jac,
       ratio_shrinking = ratios[ord][length(ord)] <= ratios[ord][1] + 1e-12,
       method = "single Jacobian applied linearly -- Hadamard would be vacuous")
}

#' Kaplan-Meier Hadamard derivative
#'
#' \eqn{\dot\Psi(h)(t) = -\int_0^t S_0(t)h(u)/S_0(u) dG(u) - L(t)h(t)}.
#' Mirrors \code{morie.fn.ksr052}.
#'
#' @param S_0,L,G,h functions.
#' @param t evaluation time.
#' @return list: derivative, integral_term, boundary_term.
#' @references Kosorok (2008), Ch. 2.
#' @examples
#' morie_km_hadamard(function(u) exp(-0.5 * u), function(u) exp(-0.3 * u),
#'                   function(u) u, function(u) 1, 1)$derivative
#' @export
morie_km_hadamard <- function(S_0, L, G, h, t) {
  t <- as.numeric(t)
  if (any(t < 0)) stop("t must be non-negative.", call. = FALSE)
  grid <- seq(0, t, length.out = 400L)
  Su <- vapply(grid, S_0, 0)
  if (any(Su <= 0)) stop("S_0 must be positive on [0, t].", call. = FALSE)
  hu <- vapply(grid, h, 0)
  Gu <- vapply(grid, G, 0)
  integrand <- S_0(t) * hu / Su
  int <- sum(0.5 * (integrand[-1] + integrand[-length(integrand)]) * diff(Gu))
  bnd <- L(t) * h(t)
  list(derivative = -int - bnd, integral_term = int, boundary_term = bnd, t = t,
       method = "Psi-dot(h)(t) = -int S0(t)h(u)/S0(u) dG(u) - L(t)h(t)")
}

#' Differentiability in quadratic mean
#'
#' \eqn{\int\[(\sqrt{dP_t}-\sqrt{dP})/t - \tfrac12 g\sqrt{dP}\]^2 \to 0}.
#' Stated on the SQUARE ROOT of the density, which is what lets kinked
#' families such as the Laplace qualify. Mirrors
#' \code{morie.fn.ksr061}.
#'
#' @param density function of (x, theta).
#' @param score candidate score at theta.
#' @param t_grid shrinking perturbations.
#' @param theta base parameter.
#' @return list: t_grid, dqm_integrals, shrinking, score_mean.
#' @references Kosorok (2008), Ch. 3.
#' @examples
#' morie_dqm_check(function(x, th) stats::dnorm(x, th), function(x) x)$shrinking
#' @export
morie_dqm_check <- function(density, score, t_grid = c(0.1, 0.05, 0.02, 0.01),
                            theta = 0) {
  if (any(t_grid <= 0)) stop("t values must be positive.", call. = FALSE)
  vals <- vapply(t_grid, function(tt) {
    f <- function(x) {
      p0 <- pmax(vapply(x, function(z) density(z, theta), 0), 0)
      pt <- pmax(vapply(x, function(z) density(z, theta + tt), 0), 0)
      sc <- vapply(x, score, 0)
      ((sqrt(pt) - sqrt(p0)) / tt - 0.5 * sc * sqrt(p0))^2
    }
    stats::integrate(f, -Inf, Inf, rel.tol = 1e-8, stop.on.error = FALSE)$value
  }, 0)
  sm <- stats::integrate(
    function(x) vapply(x, score, 0) * pmax(vapply(x, function(z) density(z, theta), 0), 0),
    -Inf, Inf, rel.tol = 1e-8, stop.on.error = FALSE
  )$value
  list(t_grid = t_grid, dqm_integrals = vals,
       shrinking = vals[length(vals)] <= vals[1] + 1e-12, score_mean = sm,
       method = "DQM on sqrt(density); covers non-differentiable densities")
}

#' Quantile Hadamard sandwich inequality
#'
#' \eqn{(F + t_n h_n)(\xi - \epsilon) \le p \le (F + t_n h_n)(\xi)}.
#' The two-sided bracket is what makes a monotone inverse
#' differentiable. Mirrors \code{morie.fn.ksr043}.
#'
#' @param F base CDF.
#' @param h_n perturbation direction.
#' @param t_n scale.
#' @param p quantile level in (0, 1).
#' @param eps_pn optional left offset.
#' @return list: lower, upper, xi_perturbed, sandwich_holds.
#' @references Kosorok (2008), Ch. 2.
#' @examples
#' morie_quantile_hadamard(stats::pnorm, function(z) 0.1 * stats::dnorm(z),
#'                         0.01, 0.7)$sandwich_holds
#' @export
morie_quantile_hadamard <- function(F, h_n, t_n, p, eps_pn = NULL) {
  p <- as.numeric(p)
  if (!isTRUE(p > 0 && p < 1)) stop("p must lie in (0, 1).", call. = FALSE)
  t_n <- as.numeric(t_n)
  if (t_n <= 0) stop("t_n must be positive.", call. = FALSE)
  eps <- if (is.null(eps_pn)) abs(t_n) else as.numeric(eps_pn)
  Fp <- function(z) F(z) + t_n * h_n(z)
  xi <- stats::uniroot(function(z) Fp(z) - p, lower = -50, upper = 50,
                       tol = 1e-10)$root
  lo <- Fp(xi - eps)
  up <- Fp(xi)
  list(lower = lo, upper = up, xi_perturbed = xi, p = p,
       sandwich_holds = lo <= p + 1e-8 && p <= up + 1e-8, eps_pn = eps,
       method = "(F + t h)(xi - eps) <= p <= (F + t h)(xi)")
}

#' Bounded-Lipschitz distance
#'
#' \eqn{\sup_{f \in BL_1}|Ef(X_n) - Ef(X)|} metrises weak convergence.
#' Estimated over sampled BL_1 members, so the value is a LOWER bound
#' on the true metric. Mirrors \code{morie.fn.ksr039}.
#'
#' @param X_n,X numeric samples.
#' @param n_functions number of BL_1 members to sample.
#' @param seed RNG seed.
#' @return list: bl_distance, is_lower_bound, n_functions.
#' @references Kosorok (2008), Ch. 2.
#' @examples
#' morie_bl_distance(rnorm(500), rnorm(500) + 2)$bl_distance
#' @export
morie_bl_distance <- function(X_n, X, n_functions = 200L, seed = 1L) {
  A <- as.numeric(X_n)
  B <- as.numeric(X)
  if (length(A) < 2L || length(B) < 2L) {
    stop("both samples need at least 2 observations.", call. = FALSE)
  }
  set.seed(seed)
  best <- 0
  for (i in seq_len(as.integer(n_functions))) {
    if (stats::runif(1) < 0.5) {
      w <- stats::runif(1, 0.1, 1)
      s <- stats::runif(1, -3, 3)
      f <- function(z) sin(w * (z - s)) / max(w, 1)
    } else {
      s <- stats::runif(1, -3, 3)
      f <- function(z) pmin(pmax(z - s, -1), 1)
    }
    best <- max(best, abs(mean(f(A)) - mean(f(B))))
  }
  list(bl_distance = best, is_lower_bound = TRUE,
       n_functions = as.integer(n_functions),
       method = "sup over sampled BL_1 functions (lower bound on the metric)")
}

#' Asymptotic tightness / equicontinuity check
#'
#' \eqn{\lim_{\delta\to0}\limsup_n P^*(\sup_{\rho<\delta}|X_n(s) -
#' X_n(t)| > \epsilon) = 0}. Tightness is the other half of weak
#' convergence: finite-dimensional convergence alone is not enough.
#' Mirrors \code{morie.fn.ksr031}.
#'
#' @param X_n matrix (replications x index points).
#' @param rho optional index values.
#' @param eps oscillation threshold.
#' @param delta_grid shrinking neighbourhood widths.
#' @return list: delta_grid, probabilities, decreasing.
#' @references Kosorok (2008), Ch. 2.
#' @examples
#' morie_tightness_check(matrix(rnorm(400), nrow = 20))$decreasing
#' @export
morie_tightness_check <- function(X_n, rho = NULL, eps = 0.1,
                                  delta_grid = c(0.5, 0.25, 0.1, 0.05)) {
  P <- as.matrix(X_n)
  npts <- ncol(P)
  if (npts < 3L) stop("need at least 3 index points.", call. = FALSE)
  tt <- if (is.null(rho)) seq(0, 1, length.out = npts) else as.numeric(rho)
  if (length(tt) != npts) stop("rho must match the index points.", call. = FALSE)
  eps <- as.numeric(eps)
  if (eps <= 0) stop("eps must be positive.", call. = FALSE)
  probs <- vapply(delta_grid, function(d) {
    if (d <= 0) stop("delta values must be positive.", call. = FALSE)
    close <- which(abs(outer(tt, tt, "-")) < d & upper.tri(diag(npts)),
                   arr.ind = TRUE)
    if (nrow(close) == 0L) {
      return(0)
    }
    osc <- apply(abs(P[, close[, 1], drop = FALSE] -
                       P[, close[, 2], drop = FALSE]), 1, max)
    mean(osc > eps)
  }, 0)
  list(delta_grid = delta_grid, probabilities = probs,
       decreasing = probs[length(probs)] <= probs[1] + 1e-12, eps = eps,
       method = "P*(modulus of continuity > eps) at shrinking delta")
}
