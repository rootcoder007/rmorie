# Optimal overlap: which subpopulation is estimable, and how to
# weight it.
# Sources: Crump, R. K., Hotz, V. J., Imbens, G. W. & Mitnik, O. A.
# (2009) Dealing with limited overlap in estimation of average
# treatment effects, Biometrika 96(1), 187-199. Theorem 5.2 (optimal
# subpopulation), Theorem 5.3 (effect on the treated), Theorem 5.4 /
# Corollaries 5.1-5.2 (optimal weights, OSATE, OWATE), Theorem 6.1
# (variance bound), section 6 (the feasible rule).
#
# Native implementation mirroring Python morie.fn.tmlefp exactly: the
# same fixed-point iteration for the variance-minimising threshold,
# the same conjugate-product computation of alpha, the same
# normalised IPW effect, the same validation messages.

#' Invert 1 / (alpha (1 - alpha)) = gamma
#'
#' Undefined below gamma = 4, where the quadratic has no real root.
#'
#' @param gamma Threshold.
#' @return alpha in (0, 1/2].
#' @references Crump, R. K. et al. (2009). Section 6.
#' @export
alpha_from_gamma <- function(gamma) {
  g <- as.numeric(gamma)
  if (g < 4) stop("tmlefp: gamma must be at least 4; below that the threshold 1/(alpha(1-alpha)) = gamma has no root in (0, 1/2]")
  root <- sqrt(1 - 4 / g)
  2 / (g * (1 + root))
}

#' Theorem 5.2: the variance-minimising trimming threshold
#'
#' Solves gamma = 2 E[k(X) | k(X) < gamma] by the fixed-point iteration
#' the equation itself defines, starting from the no-trimming point.
#' With homoskedastic variances (the default) k(x) = 1 / (e (1 - e)).
#'
#' @param pscore Propensity scores.
#' @param sigma2_treated Optional conditional variance under treatment.
#' @param sigma2_control Optional conditional variance under control.
#' @param tol Convergence tolerance.
#' @param max_iter Maximum iterations.
#' @return A list with \code{alpha}, \code{gamma}, \code{keep},
#'   \code{trim}, \code{no_trimming}, \code{k}.
#' @references Crump, R. K. et al. (2009). Theorem 5.2.
#' @export
optimal_alpha <- function(pscore, sigma2_treated = NULL,
                          sigma2_control = NULL, tol = 1e-12,
                          max_iter = 200) {
  e <- as.numeric(pscore); n <- length(e)
  if (n == 0L) stop("tmlefp: no propensity scores")
  if (any(!(e > 0 & e < 1)))
    stop("tmlefp: propensity scores must lie strictly in (0, 1)")
  if (is.null(sigma2_treated) && is.null(sigma2_control)) {
    k <- 1 / (e * (1 - e))
  } else {
    s1 <- if (is.null(sigma2_treated)) rep(1, n)
          else as.numeric(sigma2_treated)
    s0 <- if (is.null(sigma2_control)) rep(1, n)
          else as.numeric(sigma2_control)
    if (length(s1) != n || length(s0) != n)
      stop("tmlefp: one conditional variance per unit")
    if (any(s1 <= 0 | s0 <= 0))
      stop("tmlefp: conditional variances must be positive")
    k <- s1 / e + s0 / (1 - e)
  }
  mean_k <- mean(k)
  if (max(k) <= 2 * mean_k)
    return(list(alpha = 0, gamma = Inf, keep = rep(TRUE, n), trim = 0L,
                no_trimming = TRUE, k = k))
  gamma <- 2 * mean_k
  for (it in seq_len(as.integer(max_iter))) {
    sel <- k[k < gamma]
    if (length(sel) == 0L)
      stop("tmlefp: the fixed point excluded every unit; check the propensity scores")
    new <- 2 * mean(sel)
    if (abs(new - gamma) < tol * max(1, abs(gamma))) { gamma <- new; break }
    gamma <- new
  }
  keep <- k <= gamma
  homosk <- is.null(sigma2_treated) && is.null(sigma2_control)
  alpha <- if (homosk) alpha_from_gamma(gamma) else NaN
  list(alpha = alpha, gamma = gamma, keep = keep,
       trim = n - sum(keep), no_trimming = FALSE, k = k)
}

#' Theorem 5.3: the one-sided threshold for the effect on the treated
#'
#' A*_t = {x : e(x) <= alpha_t}, with alpha_t = 1 (no trimming) when
#' sup_x 1 / (1 - e(x)) <= 2 E[1 / (1 - e(X)) | W = 1], and otherwise
#' solving 1 / (1 - alpha_t) = 2 E[1 / (1 - e(X)) | W = 1, e(X) <= alpha_t].
#' Homoskedasticity only, as in the paper.
#'
#' @param pscore Propensity scores.
#' @param treated Treatment indicators.
#' @param tol Convergence tolerance.
#' @param max_iter Maximum iterations.
#' @return A list with \code{alpha_t}, \code{keep}, \code{trim},
#'   \code{no_trimming}.
#' @references Crump, R. K. et al. (2009). Theorem 5.3.
#' @export
optimal_alpha_att <- function(pscore, treated, tol = 1e-12,
                              max_iter = 200) {
  e <- as.numeric(pscore); w <- as.integer(treated); n <- length(e)
  if (n != length(w))
    stop("tmlefp: one treatment indicator per unit")
  if (any(!(e > 0 & e < 1)))
    stop("tmlefp: propensity scores must lie strictly in (0, 1)")
  idx <- which(w == 1L)
  if (length(idx) == 0L) stop("tmlefp: no treated units")
  g <- 1 / (1 - e[idx])
  if (max(1 / (1 - e)) <= 2 * mean(g))
    return(list(alpha_t = 1, keep = rep(TRUE, n), trim = 0L,
                no_trimming = TRUE))
  thr <- 2 * mean(g)
  for (it in seq_len(as.integer(max_iter))) {
    sel <- (1 / (1 - e[idx]))[1 / (1 - e[idx]) < thr]
    if (length(sel) == 0L)
      stop("tmlefp: the fixed point excluded every treated unit")
    new <- 2 * mean(sel)
    if (abs(new - thr) < tol * max(1, abs(thr))) { thr <- new; break }
    thr <- new
  }
  alpha_t <- 1 - 1 / thr
  list(alpha_t = alpha_t, keep = e <= alpha_t,
       trim = sum(e > alpha_t), no_trimming = FALSE)
}

#' Theorem 5.4 / Corollary 5.2: optimal weights
#'
#' omega*(x) = (sigma1^2 / e + sigma0^2 / (1 - e))^{-1}, which is
#' e(x)(1 - e(x)) under homoskedasticity.
#'
#' @param pscore Propensity scores.
#' @param sigma2_treated Optional conditional variance under treatment.
#' @param sigma2_control Optional conditional variance under control.
#' @return Weight vector.
#' @references Crump, R. K. et al. (2009). Theorem 5.4.
#' @export
owate_weights <- function(pscore, sigma2_treated = NULL,
                          sigma2_control = NULL) {
  e <- as.numeric(pscore)
  if (any(!(e > 0 & e < 1)))
    stop("tmlefp: propensity scores must lie strictly in (0, 1)")
  if (is.null(sigma2_treated) && is.null(sigma2_control))
    return(e * (1 - e))
  n <- length(e)
  s1 <- if (is.null(sigma2_treated)) rep(1, n)
        else as.numeric(sigma2_treated)
  s0 <- if (is.null(sigma2_control)) rep(1, n)
        else as.numeric(sigma2_control)
  1 / (s1 / e + s0 / (1 - e))
}

#' .ipw
#'
#' A step of the tmlefp_native implementation. Called by \code{morie_tmlefp}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param y A vector; its length is taken and its elements indexed.
#' @param w A vector; indexed elementwise.
#' @param e A vector; indexed elementwise.
#' @param keep Optional; may be \code{NULL}. Passed to \code{is.null}.
#' @param weights Optional; may be \code{NULL}. Passed to \code{is.null}.
#' @return A list with \code{est}, \code{n_kept}.
#' @export
.ipw <- function(y, w, e, keep = NULL, weights = NULL) {
  n <- length(y)
  sel <- if (is.null(keep)) seq_len(n) else which(keep)
  if (length(sel) == 0L) stop("tmlefp: the selected subpopulation is empty")
  om <- if (is.null(weights)) rep(1, n) else weights
  num1 <- sum(om[sel] * w[sel] * y[sel] / e[sel])
  den1 <- sum(om[sel] * w[sel] / e[sel])
  num0 <- sum(om[sel] * (1 - w[sel]) * y[sel] / (1 - e[sel]))
  den0 <- sum(om[sel] * (1 - w[sel]) / (1 - e[sel]))
  if (den1 <= 0 || den0 <= 0)
    stop("tmlefp: the subpopulation has no treated or no control units")
  list(est = num1 / den1 - num0 / den0, n_kept = length(sel))
}

#' Optimal-overlap estimands and their trimming rules
#'
#' @param y Outcome vector.
#' @param treatment Treatment indicator.
#' @param pscore Propensity scores, strictly inside (0, 1).
#' @param sigma2_treated Optional conditional variance under treatment.
#' @param sigma2_control Optional conditional variance under control.
#' @param estimand One of \code{"ate"}, \code{"att"}.
#' @return A list with \code{estimate}, \code{osate}, \code{ate_full},
#'   \code{owate}, \code{owate_weights}, \code{alpha}, \code{gamma},
#'   \code{keep}, \code{n}, \code{n_kept}, \code{n_trimmed},
#'   \code{no_trimming}, \code{variance_bound},
#'   \code{variance_bound_full}, \code{estimand}, \code{note},
#'   \code{method}.
#' @references Crump, R. K. et al. (2009).
#' @export
morie_tmlefp <- function(y, treatment, pscore,
                          sigma2_treated = NULL,
                          sigma2_control = NULL,
                          estimand = "ate") {
  yv <- as.numeric(y); w <- as.integer(treatment)
  e <- as.numeric(pscore); n <- length(yv)
  if (!(n == length(w) && length(w) == length(e)))
    stop("tmlefp: y, treatment and pscore must have the same length")
  if (any(!(w %in% c(0L, 1L))))
    stop("tmlefp: treatment must be 0 or 1")
  if (!estimand %in% c("ate", "att"))
    stop("tmlefp: estimand must be 'ate' or 'att'")
  if (estimand == "ate") {
    rule <- optimal_alpha(e, sigma2_treated, sigma2_control)
    keep <- rule$keep
    alpha <- rule$alpha
    gamma <- rule$gamma
  } else {
    rule <- optimal_alpha_att(e, w)
    keep <- rule$keep
    alpha <- rule$alpha_t
    gamma <- NaN
  }
  k <- if (is.null(sigma2_treated) && is.null(sigma2_control))
         1 / (e * (1 - e)) else rule$k
  bound <- function(sel) {
    m <- k[sel]
    if (length(m) == 0L) return(Inf)
    q <- length(m) / n
    (sum(m) / length(m)) / q
  }
  ipw_keep <- .ipw(yv, w, e, keep)
  ipw_full <- .ipw(yv, w, e, NULL)
  om <- owate_weights(e, sigma2_treated, sigma2_control)
  ipw_owate <- .ipw(yv, w, e, NULL, om)
  list(estimate = ipw_keep$est, osate = ipw_keep$est,
       ate_full = ipw_full$est, owate = ipw_owate$est,
       owate_weights = om, alpha = alpha, gamma = gamma, keep = keep,
       n = n, n_kept = ipw_keep$n_kept,
       n_trimmed = n - ipw_keep$n_kept,
       no_trimming = rule$no_trimming,
       variance_bound = bound(keep),
       variance_bound_full = bound(rep(TRUE, n)),
       estimand = estimand,
       note = paste0("the estimand CHANGES with the rule: this is the ",
                     "effect for the subpopulation kept, not for the ",
                     "whole population (Crump et al. 2009, section 5)"),
       method = "optimal-overlap subpopulation and weights (Crump, Hotz, Imbens & Mitnik 2009)")
}

#' Compact alias per ledger/NAMING.md
#' @export
morie_optimal_overlap <- morie_tmlefp

#' Name carried over from the generated stub this replaced
#' @export
morie_tmle_effective_pi <- morie_tmlefp
