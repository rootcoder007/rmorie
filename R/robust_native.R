# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Robust regression and robust scale.
#
# R mirror of morie.fn.{qnsc,snsc,hubrr,mestrg,sestrg,mmreg,mmestr,
# taubrg,theils,sensSlp} and the _robust helper.
#
# The organising trade-off is BREAKDOWN against EFFICIENCY, and none
# of the constants below is folklore: 1.345 solves Huber's 95%
# efficiency equation, 1.5476 makes E_Phi[rho] = 1/2 for the biweight
# (the 50%-breakdown calibration), 4.685 is the biweight's 95%
# constant, 2.2191 = 1/(sqrt(2) qnorm(5/8)) is Qn's, and 1.1926 Sn's.
# The Python tests recompute each from its defining equation.

.rob_huber_c <- 1.345
.rob_tukey_c_bdp <- 1.5476
.rob_tukey_c_eff <- 4.685
.rob_qn_d <- 1 / (sqrt(2) * stats::qnorm(5 / 8))
.rob_sn_c <- 1.1926

#' .rob_mad
#'
#' Part of the robust_native implementation; see the file header for the
#' source it follows.
#'
#' @param r See Usage.
#' @return A numeric value.
#' @export
.rob_mad <- function(r) {
  1.482602218505602 * stats::median(abs(r - stats::median(r)))
}

#' .rob_tukey_rho
#'
#' Part of the robust_native implementation; see the file header for the
#' source it follows.
#'
#' @param u See Usage.
#' @param cc See Usage.
#' @return A numeric value.
#' @export
.rob_tukey_rho <- function(u, cc) {
  v <- pmin(pmax(u / cc, -1), 1)
  1 - (1 - v^2)^3
}

# w(u) = psi(u)/u: (1 - (u/c)^2)^2 inside, ZERO outside. The
# redescending part is what buys breakdown -- a gross outlier gets no
# vote at all, where Huber's psi still gives it a bounded one.
#' W(u) = psi(u)/u: (1 - (u/c)^2)^2 inside, ZERO outside. The
#'
#' redescending part is what buys breakdown -- a gross outlier gets no
#' vote at all, where Huber\'s psi still gives it a bounded one.
#'
#' @param u See Usage.
#' @param cc See Usage.
#' @return The value of \code{ifelse}.
#' @export
.rob_tukey_w <- function(u, cc) {
  v <- u / cc
  ifelse(abs(v) < 1, (1 - v^2)^2, 0)
}

# The M-scale: s solving mean(rho(r/s)) = b, biweight at c = 1.5476,
# b = 1/2 -- the 50%-breakdown calibration. The fixed-point iteration
# s^2 <- s^2 * mean(rho(r/s))/b is monotone for the biweight.
#' The M-scale: s solving mean(rho(r/s)) = b, biweight at c = 1.5476,
#'
#' b = 1/2 -- the 50%-breakdown calibration. The fixed-point iteration
#' s^2 <- s^2 * mean(rho(r/s))/b is monotone for the biweight.
#'
#' @param r See Usage.
#' @param cc Defaults to \code{.rob_tukey_c_bdp}.
#' @param b Defaults to \code{0.5}.
#' @return The value of \code{s}, as built in the body.
#' @export
.rob_s_scale <- function(r, cc = .rob_tukey_c_bdp, b = 0.5) {
  s <- .rob_mad(r)
  if (s <= 0) s <- mean(abs(r))
  if (s <= 0) return(0)
  for (i in seq_len(200L)) {
    m <- mean(.rob_tukey_rho(r / s, cc))
    if (m <= 0) return(0)
    new <- s * sqrt(m / b)
    if (abs(new - s) < 1e-12 * s) return(new)
    s <- new
  }
  s
}

#' .rob_design
#'
#' Part of the robust_native implementation; see the file header for the
#' source it follows.
#'
#' @param X See Usage.
#' @param y See Usage.
#' @return A list with \code{X}, \code{y}.
#' @export
.rob_design <- function(X, y) {
  yv <- as.numeric(y)
  A <- as.matrix(X)
  storage.mode(A) <- "double"
  if (nrow(A) != length(yv)) A <- t(A)
  if (nrow(A) != length(yv)) {
    stop(sprintf("X has %d rows for %d responses.", nrow(A), length(yv)),
         call. = FALSE)
  }
  if (!any(apply(A, 2L, function(cc) isTRUE(all.equal(cc, rep(1, nrow(A))))))) {
    A <- cbind(1, A)
  }
  list(X = A, y = yv)
}

# S-estimator core: random p-subsets ranked by residual M-scale, then
# local IRLS refinement. Non-convex objective, so the subsets are the
# global search and the refinement the local one.
#' S-estimator core: random p-subsets ranked by residual M-scale, then
#'
#' local IRLS refinement. Non-convex objective, so the subsets are the
#' global search and the refinement the local one.
#'
#' @param X See Usage.
#' @param y See Usage.
#' @param n_subsets See Usage.
#' @param seed See Usage.
#' @return A list with \code{beta}, \code{scale}.
#' @export
.rob_s_reg <- function(X, y, n_subsets, seed) {
  n <- nrow(X)
  p <- ncol(X)
  if (n <= p) {
    stop(sprintf("need more observations than parameters, got n = %d, p = %d.",
                 n, p), call. = FALSE)
  }
  old <- if (exists(".Random.seed", envir = globalenv())) {
    get(".Random.seed", envir = globalenv())
  } else NULL
  set.seed(seed)
  on.exit(if (!is.null(old)) assign(".Random.seed", old, envir = globalenv()))
  best_s <- Inf
  best_beta <- NULL
  for (i in seq_len(n_subsets)) {
    idx <- sample.int(n, p)
    sub <- X[idx, , drop = FALSE]
    if (qr(sub)$rank < p) next
    beta <- tryCatch(solve(sub, y[idx]), error = function(e) NULL)
    if (is.null(beta)) next
    sc <- .rob_s_scale(y - as.numeric(X %*% beta))
    if (sc > 0 && sc < best_s) {
      best_s <- sc
      best_beta <- beta
    }
  }
  if (is.null(best_beta)) {
    stop("no non-singular p-subset was found; the design is rank-deficient.",
         call. = FALSE)
  }
  beta <- best_beta
  for (i in seq_len(50L)) {
    r <- y - as.numeric(X %*% beta)
    w <- .rob_tukey_w(r / best_s, .rob_tukey_c_bdp)
    if (!any(w > 0)) break
    Aw <- X * w
    beta_new <- tryCatch(
      as.numeric(qr.coef(qr(crossprod(Aw, X)), crossprod(Aw, y))),
      error = function(e) beta)
    sc <- .rob_s_scale(y - as.numeric(X %*% beta_new))
    if (sc >= best_s - 1e-12) break
    best_s <- sc
    beta <- beta_new
  }
  list(beta = beta, scale = best_s)
}

#' .rob_irls_fixed_scale
#'
#' Part of the robust_native implementation; see the file header for the
#' source it follows.
#'
#' @param X See Usage.
#' @param y See Usage.
#' @param beta See Usage.
#' @param scale See Usage.
#' @param cc See Usage.
#' @return A list with \code{beta}, \code{converged}.
#' @export
.rob_irls_fixed_scale <- function(X, y, beta, scale, cc) {
  conv <- FALSE
  for (i in seq_len(100L)) {
    r <- y - as.numeric(X %*% beta)
    w <- .rob_tukey_w(r / scale, cc)
    if (!any(w > 0)) break
    Aw <- X * w
    beta_new <- as.numeric(qr.coef(qr(crossprod(Aw, X)), crossprod(Aw, y)))
    if (max(abs(beta_new - beta)) < 1e-10 * (1 + max(abs(beta)))) {
      beta <- beta_new
      conv <- TRUE
      break
    }
    beta <- beta_new
  }
  list(beta = beta, converged = conv)
}


#' Qn robust scale estimator
#'
#' Rousseeuw and Croux (1993): `Qn = d * {|x_i - x_j|, i < j}_(k)`,
#' the k-th order statistic of the pairwise absolute differences with
#' `k = choose(h, 2)`, `h = floor(n/2) + 1`, and
#' `d = 1/(sqrt(2) qnorm(5/8))` for normal consistency.
#'
#' Qn answers a specific complaint about the MAD: the MAD is built
#' around a location and aimed at symmetric distributions, with 37%
#' normal efficiency. Qn uses no location at all, keeps the 50%
#' breakdown, and reaches 82%. The paper's finite-sample correction
#' factors are applied; without them Qn is biased low in exactly the
#' small samples robust scales get used on.
#'
#' @param x numeric sample, at least 2 values.
#' @return list: value, k, h, d, correction, breakdown,
#'   gaussian_efficiency, location_free, n, method.
#' @references Rousseeuw and Croux (1993), *JASA* 88:1273-1283.
#' @examples
#' morie_rob_qn(stats::rnorm(50))$value
#' @export
morie_rob_qn <- function(x) {
  xv <- as.numeric(x)
  n <- length(xv)
  if (n < 2L) {
    stop(sprintf("need at least 2 observations, got %d.", n), call. = FALSE)
  }
  h <- n %/% 2L + 1L
  k <- h * (h - 1L) / 2L
  D <- abs(outer(xv, xv, "-"))
  diffs <- D[upper.tri(D)]
  stat <- sort(diffs, partial = k)[k]
  small <- c(`2` = 0.399, `3` = 0.994, `4` = 0.512, `5` = 0.844,
             `6` = 0.611, `7` = 0.857, `8` = 0.669, `9` = 0.872)
  corr <- if (n <= 9L) small[[as.character(n)]] else
    if (n %% 2L == 1L) n / (n + 1.4) else n / (n + 3.8)
  list(value = .rob_qn_d * corr * stat, k = k, h = h, d = .rob_qn_d,
       correction = corr, breakdown = 0.5, gaussian_efficiency = 0.82,
       location_free = TRUE, n = n,
       method = "Qn = d * k-th order statistic of pairwise |differences| (Rousseeuw-Croux 1993)")
}


#' Sn robust scale estimator
#'
#' Rousseeuw and Croux (1993): `Sn = c * lomed_i himed_j |x_i - x_j|`
#' with `c = 1.1926`. The inner high median is the `(n %/% 2 + 1)`-th
#' order statistic of the n values including the diagonal zero, the
#' outer low median the `floor((n+1)/2)`-th. The convention was
#' CALIBRATED against the paper's own small-sample correction factors
#' rather than trusted: only this index makes the corrected estimator
#' unbiased at every n, and one higher is 12-40% high at small n.
#'
#' Location-free, 50% breakdown, 58% normal efficiency -- between the
#' MAD's 37% and Qn's 82%.
#'
#' @param x numeric sample, at least 2 values.
#' @return list: value, c, correction, breakdown,
#'   gaussian_efficiency, location_free, n, method.
#' @references Rousseeuw and Croux (1993), *JASA* 88:1273-1283,
#'   Sec. 2; Croux and Rousseeuw (1992) for the O(n log n) algorithm.
#' @examples
#' morie_rob_sn(stats::rnorm(50))$value
#' @export
morie_rob_sn <- function(x) {
  xv <- as.numeric(x)
  n <- length(xv)
  if (n < 2L) {
    stop(sprintf("need at least 2 observations, got %d.", n), call. = FALSE)
  }
  D <- abs(outer(xv, xv, "-"))
  inner <- apply(D, 1L, function(row) sort(row)[n %/% 2L + 1L])
  stat <- sort(inner)[(n + 1L) %/% 2L]
  small <- c(`2` = 0.743, `3` = 1.851, `4` = 0.954, `5` = 1.351,
             `6` = 0.993, `7` = 1.198, `8` = 1.005, `9` = 1.131)
  corr <- if (n <= 9L) small[[as.character(n)]] else
    if (n %% 2L == 1L) n / (n - 0.9) else 1
  list(value = .rob_sn_c * corr * stat, c = .rob_sn_c, correction = corr,
       breakdown = 0.5, gaussian_efficiency = 0.58, location_free = TRUE,
       n = n,
       method = "Sn = c * lomed_i himed_j |x_i - x_j| (Rousseeuw-Croux 1993)")
}


#' Huber M-estimator regression
#'
#' Huber (1973) by IRLS with the residual MAD as the re-estimated
#' scale. `c = 1.345` solves the 95%-efficiency equation at the
#' normal. Huber's psi bounds the influence of a RESIDUAL, not of a
#' design row: its regression breakdown point is 0, a bad-leverage
#' cluster still ruins it, and that documented failure is a test.
#' The S/MM estimators ([morie_rob_mm()]) are the fix.
#'
#' @param X design; a constant column is added when absent.
#' @param y response.
#' @param c tuning constant; 1.345 when `NULL`.
#' @param max_iter IRLS iterations.
#' @return list: beta, scale, se, residuals, weights, c, converged,
#'   breakdown, n, p, method.
#' @references Huber (1973), *Annals of Statistics* 1:799-821;
#'   Huber (1964), *Ann. Math. Statist.* 35:73-101.
#' @examples
#' x <- stats::rnorm(50)
#' morie_rob_huber(x, 1 + 2 * x + stats::rnorm(50))$beta
#' @export
morie_rob_huber <- function(X, y, c = NULL, max_iter = 100L) {
  d <- .rob_design(X, y)
  A <- d$X
  yv <- d$y
  n <- nrow(A)
  p <- ncol(A)
  if (n <= p) {
    stop(sprintf("need more observations than parameters, got n = %d, p = %d.",
                 n, p), call. = FALSE)
  }
  cc <- if (is.null(c)) .rob_huber_c else as.numeric(c)
  if (cc <= 0) stop(sprintf("c must be positive, got %g.", cc), call. = FALSE)
  beta <- as.numeric(qr.coef(qr(A), yv))
  conv <- FALSE
  scale <- 1
  for (i in seq_len(as.integer(max_iter))) {
    r <- yv - as.numeric(A %*% beta)
    scale <- .rob_mad(r)
    if (scale <= 0) {
      conv <- TRUE
      break
    }
    u <- r / scale
    w <- ifelse(abs(u) <= cc, 1, cc / abs(u))
    Aw <- A * w
    beta_new <- as.numeric(qr.coef(qr(crossprod(Aw, A)), crossprod(Aw, yv)))
    if (max(abs(beta_new - beta)) < 1e-10 * (1 + max(abs(beta)))) {
      beta <- beta_new
      conv <- TRUE
      break
    }
    beta <- beta_new
  }
  r <- yv - as.numeric(A %*% beta)
  # Recompute the scale from the FINAL residuals.  It was left at the
  # value from the previous iterate, which also propagated into `se`
  # below (se = sqrt(diag * kappa * scale^2)).  No MASS-parity contract
  # here -- this is Huber (1964, 1973) -- so the scale should simply
  # describe the residuals actually returned.
  if (scale > 0) scale <- .rob_mad(r)
  u <- if (scale > 0) r / scale else r
  w <- ifelse(abs(u) <= cc, 1, cc / pmax(abs(u), 1e-300))
  psi <- pmin(pmax(u, -cc), cc)
  dpsi <- as.numeric(abs(u) <= cc)
  kappa <- mean(psi^2) / max(mean(dpsi)^2, 1e-12)
  XtX_inv <- solve(crossprod(A))
  se <- sqrt(pmax(diag(XtX_inv) * kappa * scale^2, 0))
  list(beta = beta, scale = scale, se = se, residuals = r, weights = w,
       c = cc, converged = conv, breakdown = 0,
       bounded_influence_in = paste("the residual only -- NOT the design; a",
                                    "bad leverage cluster still breaks it,",
                                    "which is what the S/MM estimators fix"),
       n = n, p = p,
       method = "Huber M-regression by IRLS, c = 1.345 for 95% normal efficiency")
}


#' General M-estimator regression
#'
#' IRLS with a choice of psi: `"huber"` (monotone, unique solution,
#' default `c = 1.345`) or `"bisquare"` (redescending, default
#' `c = 4.685`). A monotone psi converges from anywhere but grants
#' every observation a non-zero vote; a redescending psi zeroes gross
#' outliers but its objective is non-convex, so IRLS from least
#' squares finds a LOCAL solution -- for high-breakdown behaviour use
#' [morie_rob_mm()], and the output says so.
#'
#' @param X,y design and response.
#' @param psi `"huber"` or `"bisquare"`.
#' @param c tuning constant; the family's 95% value when `NULL`.
#' @param max_iter IRLS iterations.
#' @return list: beta, scale, residuals, weights, psi, c, monotone,
#'   unique_solution, converged, start_dependent_warning, n, p,
#'   method.
#' @references Huber (1973), *Annals of Statistics* 1:799-821;
#'   Beaton and Tukey (1974), *Technometrics* 16:147-185.
#' @examples
#' x <- stats::rnorm(50)
#' morie_rob_m(x, 1 + 2 * x + stats::rnorm(50), psi = "bisquare")$beta
#' @export
morie_rob_m <- function(X, y, psi = "huber", c = NULL, max_iter = 100L) {
  if (!psi %in% c("huber", "bisquare")) {
    stop("psi must be 'huber' or 'bisquare'.", call. = FALSE)
  }
  d <- .rob_design(X, y)
  A <- d$X
  yv <- d$y
  n <- nrow(A)
  p <- ncol(A)
  if (n <= p) {
    stop(sprintf("need more observations than parameters, got n = %d, p = %d.",
                 n, p), call. = FALSE)
  }
  cc <- if (!is.null(c)) as.numeric(c) else
    if (identical(psi, "huber")) .rob_huber_c else .rob_tukey_c_eff
  if (cc <= 0) stop(sprintf("c must be positive, got %g.", cc), call. = FALSE)
  wfun <- function(u) {
    if (identical(psi, "huber")) {
      au <- pmax(abs(u), 1e-300)
      ifelse(au <= cc, 1, cc / au)
    } else {
      .rob_tukey_w(u, cc)
    }
  }
  beta <- as.numeric(qr.coef(qr(A), yv))
  conv <- FALSE
  scale <- 1
  for (i in seq_len(as.integer(max_iter))) {
    r <- yv - as.numeric(A %*% beta)
    scale <- .rob_mad(r)
    if (scale <= 0) {
      conv <- TRUE
      break
    }
    w <- wfun(r / scale)
    if (!any(w > 0)) break
    Aw <- A * w
    beta_new <- as.numeric(qr.coef(qr(crossprod(Aw, A)), crossprod(Aw, yv)))
    if (max(abs(beta_new - beta)) < 1e-10 * (1 + max(abs(beta)))) {
      beta <- beta_new
      conv <- TRUE
      break
    }
    beta <- beta_new
  }
  r <- yv - as.numeric(A %*% beta)
  # Same one-iteration lag as morie_rob_huber above; here it also made
  # the reported weights inconsistent with the reported scale.
  if (scale > 0) scale <- .rob_mad(r)
  list(beta = beta, scale = scale, residuals = r,
       weights = if (scale > 0) wfun(r / scale) else rep(1, n),
       psi = psi, c = cc,
       monotone = identical(psi, "huber"),
       unique_solution = identical(psi, "huber"),
       converged = conv,
       start_dependent_warning = if (identical(psi, "huber")) NULL else
         paste("the biweight objective is non-convex: IRLS from least",
               "squares finds a LOCAL solution; for high-breakdown behaviour",
               "use morie_rob_mm instead"),
       n = n, p = p,
       method = sprintf("M-regression by IRLS, %s psi at c = %g", psi, cc))
}


#' S-estimator regression
#'
#' Rousseeuw and Yohai (1984): minimise the residual M-scale, biweight
#' at `c = 1.5476` with `b = 1/2` -- the calibration that makes the
#' breakdown point 50%. The price is 28.7% normal efficiency, which
#' is why the S-estimate is a STARTING POINT: its scale and
#' coefficients seed [morie_rob_mm()], which recovers 95% efficiency
#' without giving the breakdown back. Computation is random p-subsets
#' plus local IRLS refinement, reproducible via `seed`.
#'
#' @param X,y design and response.
#' @param n_subsets random p-subsets to try.
#' @param seed subset seed.
#' @return list: beta, scale, residuals, breakdown,
#'   gaussian_efficiency, c, b, role, n_subsets, n, p, method.
#' @references Rousseeuw and Yohai (1984), Lecture Notes in
#'   Statistics 26, Springer, 256-272.
#' @examples
#' x <- stats::rnorm(60)
#' morie_rob_s(x, 1 + 2 * x + stats::rnorm(60), n_subsets = 50)$beta
#' @export
morie_rob_s <- function(X, y, n_subsets = 200L, seed = 0) {
  d <- .rob_design(X, y)
  r <- .rob_s_reg(d$X, d$y, as.integer(n_subsets), seed)
  list(beta = r$beta, scale = r$scale,
       residuals = d$y - as.numeric(d$X %*% r$beta),
       breakdown = 0.5, gaussian_efficiency = 0.287,
       c = .rob_tukey_c_bdp, b = 0.5,
       role = paste("a starting point: seed the MM step for 95% efficiency",
                    "without giving the 50% breakdown back"),
       n_subsets = as.integer(n_subsets),
       n = nrow(d$X), p = ncol(d$X),
       method = "S-estimator: minimise the residual M-scale (Rousseeuw-Yohai 1984)")
}


#' MM-estimator regression
#'
#' Yohai (1987), the three-stage construction with 50% breakdown AND
#' 95% normal efficiency: a high-breakdown initial fit, the M-scale
#' of its residuals (biweight `c = 1.5476`, owns the breakdown), then
#' an M-step at `c = 4.685` iterated **with the scale held fixed**.
#' Holding the scale fixed is the load-bearing detail: re-estimating
#' it under the larger c would let outliers back into the scale and
#' the breakdown would fall back toward zero.
#'
#' @param X,y design and response.
#' @param n_subsets random p-subsets for the initial stage.
#' @param seed subset seed.
#' @return list: beta, scale, beta_initial, residuals, se, weights,
#'   breakdown, gaussian_efficiency, scale_held_fixed, converged, n,
#'   p, method.
#' @references Yohai (1987), *Annals of Statistics* 15:642-656,
#'   Sec. 2 and Theorem 2.1.
#' @examples
#' x <- stats::rnorm(60)
#' morie_rob_mm(x, 1 + 2 * x + stats::rnorm(60), n_subsets = 50)$beta
#' @export
morie_rob_mm <- function(X, y, n_subsets = 200L, seed = 0) {
  d <- .rob_design(X, y)
  A <- d$X
  yv <- d$y
  s <- .rob_s_reg(A, yv, as.integer(n_subsets), seed)
  m <- .rob_irls_fixed_scale(A, yv, s$beta, s$scale, .rob_tukey_c_eff)
  beta <- m$beta
  r <- yv - as.numeric(A %*% beta)
  u <- if (s$scale > 0) r / s$scale else r
  w <- .rob_tukey_w(u, .rob_tukey_c_eff)
  v <- u / .rob_tukey_c_eff
  dpsi_mean <- mean((1 - v^2) * (1 - 5 * v^2) * (abs(v) < 1))
  kappa <- mean((u * w)^2) / max(dpsi_mean^2, 1e-12)
  XtX_inv <- solve(crossprod(A))
  se <- sqrt(pmax(diag(XtX_inv) * kappa * s$scale^2, 0))
  list(beta = beta, scale = s$scale, beta_initial = s$beta,
       residuals = r, se = se, weights = w,
       breakdown = 0.5, gaussian_efficiency = 0.95,
       scale_held_fixed = TRUE,
       why_fixed = paste("re-estimating the scale in the efficiency stage",
                         "would let outliers back into it through the larger",
                         "c, and the breakdown would fall back toward zero"),
       converged = m$converged,
       n = nrow(A), p = ncol(A),
       method = "MM-estimator (Yohai 1987): S-scale at c = 1.5476, M-step at c = 4.685, scale fixed")
}


#' MM-estimator regression -- alias entry point
#'
#' One estimator, two catalogue entries: the computation is
#' [morie_rob_mm()], invoked directly so the two cannot drift apart.
#'
#' @param X,y design and response.
#' @param n_subsets,seed as in [morie_rob_mm()].
#' @return the [morie_rob_mm()] list plus `alias_of`.
#' @references Yohai (1987), *Annals of Statistics* 15:642-656.
#' @examples
#' x <- stats::rnorm(50)
#' morie_rob_mm_alias(x, 2 * x + stats::rnorm(50), n_subsets = 50)$alias_of
#' @export
morie_rob_mm_alias <- function(X, y, n_subsets = 200L, seed = 0) {
  out <- morie_rob_mm(X, y, n_subsets = n_subsets, seed = seed)
  out$alias_of <- "morie_rob_mm"
  out
}


#' Tau-estimator regression
#'
#' Yohai and Zamar (1988): minimise the TAU-SCALE
#' `tau^2 = s^2 * mean(rho_2(r/s))/b_2`, where `s` is the M-scale
#' under the tight `rho_1` (biweight `c1 = 1.5476`, 50% breakdown)
#' and `rho_2` a wide biweight (`c2 = 6.08`, ~95% efficiency). Where
#' MM freezes an S-scale and re-fits beta, the tau-estimator bakes
#' both rhos into ONE objective, and the tau-scale is itself a
#' robust, efficiency-calibrated residual scale.
#'
#' @param X,y design and response.
#' @param n_subsets random p-subsets.
#' @param seed subset seed.
#' @param c1,c2 the two biweight constants.
#' @return list: beta, tau_scale, m_scale, residuals, breakdown,
#'   gaussian_efficiency, c1, c2, versus_mm, n, p, method.
#' @references Yohai and Zamar (1988), *JASA* 83:406-413, Secs. 2
#'   and 4.
#' @examples
#' x <- stats::rnorm(60)
#' morie_rob_tau(x, 1 + 2 * x + stats::rnorm(60), n_subsets = 50)$beta
#' @export
morie_rob_tau <- function(X, y, n_subsets = 200L, seed = 0,
                          c1 = 1.5476, c2 = 6.08) {
  d <- .rob_design(X, y)
  A <- d$X
  yv <- d$y
  n <- nrow(A)
  p <- ncol(A)
  if (n <= p) {
    stop(sprintf("need more observations than parameters, got n = %d, p = %d.",
                 n, p), call. = FALSE)
  }
  c1 <- as.numeric(c1)
  c2 <- as.numeric(c2)
  b2 <- stats::integrate(function(u) .rob_tukey_rho(u, c2) * stats::dnorm(u),
                         -12, 12)$value
  tau_of <- function(beta) {
    r <- yv - as.numeric(A %*% beta)
    s <- .rob_s_scale(r, c1, 0.5)
    if (s <= 0) return(c(0, 0))
    c(sqrt(max(s^2 * mean(.rob_tukey_rho(r / s, c2)) / b2, 0)), s)
  }
  old <- if (exists(".Random.seed", envir = globalenv())) {
    get(".Random.seed", envir = globalenv())
  } else NULL
  set.seed(seed)
  on.exit(if (!is.null(old)) assign(".Random.seed", old, envir = globalenv()))
  best <- list(tau = Inf, beta = NULL, s = 0)
  for (i in seq_len(as.integer(n_subsets))) {
    idx <- sample.int(n, p)
    sub <- A[idx, , drop = FALSE]
    if (qr(sub)$rank < p) next
    beta <- tryCatch(solve(sub, yv[idx]), error = function(e) NULL)
    if (is.null(beta)) next
    ts <- tau_of(beta)
    if (ts[1L] > 0 && ts[1L] < best$tau) {
      best <- list(tau = ts[1L], beta = beta, s = ts[2L])
    }
  }
  if (is.null(best$beta)) {
    stop("no non-singular p-subset was found.", call. = FALSE)
  }
  tau <- best$tau
  beta <- best$beta
  s <- best$s
  for (i in seq_len(50L)) {
    r <- yv - as.numeric(A %*% beta)
    s <- .rob_s_scale(r, c1, 0.5)
    if (s <= 0) break
    u <- r / s
    w <- .rob_tukey_w(u, c1) + .rob_tukey_w(u, c2)
    Aw <- A * w
    beta_new <- as.numeric(qr.coef(qr(crossprod(Aw, A)), crossprod(Aw, yv)))
    tn <- tau_of(beta_new)
    if (tn[1L] >= tau - 1e-12) break
    tau <- tn[1L]
    beta <- beta_new
  }
  list(beta = beta, tau_scale = tau, m_scale = s,
       residuals = yv - as.numeric(A %*% beta),
       breakdown = 0.5, gaussian_efficiency = 0.95, c1 = c1, c2 = c2,
       versus_mm = paste("MM freezes an S-scale and re-fits beta; the",
                         "tau-estimator bakes both rhos into ONE objective,",
                         "and the tau-scale is itself a robust efficient",
                         "residual scale"),
       n = n, p = p,
       method = "Tau-estimator (Yohai-Zamar 1988): minimise the efficient tau-scale")
}


#' Theil-Sen slope estimator
#'
#' The median of all pairwise slopes, with the median-residual
#' intercept (Theil 1950; Sen 1968). Pairs with tied `x` are
#' EXCLUDED, as Sen specifies -- their slope is undefined, and
#' silently treating them as 0 or Inf biases the median. Breakdown is
#' `1 - 1/sqrt(2)`, about 29.3%: the median of `choose(n, 2)` slopes
#' fails only once the contaminated PAIRS are a majority. Sen's
#' Sec. 5 interval reads the CI off the ordered pairwise slopes at
#' Kendall-tau ranks, so no residual variance is estimated.
#'
#' @param x,y paired observations.
#' @param alpha miss probability for Sen's interval.
#' @return list: slope, intercept, ci, n_pairs, n_tied_x, breakdown,
#'   ci_method, n, method.
#' @references Theil (1950), *Proc. KNAW* 53; Sen (1968), *JASA*
#'   63:1379-1389, Secs. 3 and 5.
#' @examples
#' x <- stats::rnorm(30)
#' morie_rob_theil_sen(x, 2 * x + stats::rnorm(30))$slope
#' @export
morie_rob_theil_sen <- function(x, y, alpha = 0.05) {
  xv <- as.numeric(x)
  yv <- as.numeric(y)
  if (length(xv) != length(yv)) {
    stop(sprintf("x has %d entries and y has %d.", length(xv), length(yv)),
         call. = FALSE)
  }
  n <- length(xv)
  if (n < 3L) {
    stop(sprintf("need at least 3 observations, got %d.", n), call. = FALSE)
  }
  a <- as.numeric(alpha)
  if (a <= 0 || a >= 1) {
    stop(sprintf("alpha must lie in (0, 1), got %g.", a), call. = FALSE)
  }
  ij <- which(upper.tri(matrix(0, n, n)), arr.ind = TRUE)
  dx <- xv[ij[, 2L]] - xv[ij[, 1L]]
  dy <- yv[ij[, 2L]] - yv[ij[, 1L]]
  ok <- dx != 0
  n_tied <- sum(!ok)
  if (!any(ok)) {
    stop("every pair of x values is tied; no slope is defined.",
         call. = FALSE)
  }
  slopes <- sort(dy[ok] / dx[ok])
  N <- length(slopes)
  slope <- stats::median(slopes)
  intercept <- stats::median(yv - slope * xv)
  var_s <- n * (n - 1) * (2 * n + 5) / 18
  cval <- stats::qnorm(1 - a / 2) * sqrt(var_s)
  m1 <- max(floor((N - cval) / 2), 0) + 1L
  # the (m2 + 1)-th order statistic, matching the Python
  # module's 0-based slopes[min(m2, N-1)]
  m2 <- min(ceiling((N + cval) / 2) + 1L, N)
  list(slope = slope, intercept = intercept,
       ci = c(slopes[m1], slopes[m2]),
       n_pairs = N, n_tied_x = n_tied,
       breakdown = 1 - 1 / sqrt(2),
       ci_method = paste("Sen (1968) Sec. 5: order statistics of the",
                         "pairwise slopes at Kendall-tau ranks; no residual",
                         "variance is estimated"),
       n = n,
       method = "Theil-Sen: median of pairwise slopes, median-residual intercept")
}


#' Sen's slope for a time series
#'
#' The Theil-Sen estimator applied to a series against its time
#' index, the form in which hydrology and climatology cite Sen
#' (1968). One implementation: the computation is
#' [morie_rob_theil_sen()] on `(t, y)`. The addition is the trend
#' reading, stated only when Sen's interval excludes zero.
#'
#' @param y series values.
#' @param t time index; `0..n-1` when `NULL`.
#' @param alpha miss probability.
#' @return the [morie_rob_theil_sen()] list plus trend, per,
#'   alias_of.
#' @references Sen (1968), *JASA* 63:1379-1389; Theil (1950);
#'   Mann (1945), *Econometrica* 13:245-259.
#' @examples
#' morie_rob_sens_slope(cumsum(stats::rnorm(30, 0.5)))$trend
#' @export
morie_rob_sens_slope <- function(y, t = NULL, alpha = 0.05) {
  yv <- as.numeric(y)
  tv <- if (is.null(t)) seq_along(yv) - 1 else as.numeric(t)
  out <- morie_rob_theil_sen(tv, yv, alpha = alpha)
  out$trend <- if (out$ci[1L] > 0) "increasing" else
    if (out$ci[2L] < 0) "decreasing" else "no trend at this alpha"
  out$per <- if (is.null(t)) "time step" else "unit of t"
  out$alias_of <- "morie_rob_theil_sen"
  out$method <- "Sen's slope: Theil-Sen against the time index"
  out
}
