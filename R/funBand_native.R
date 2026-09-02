# funBand.R -- function file (rootcoder007/morie)
# Bayesian confidence intervals for the cross-validated smoothing spline.
#
# Theorem 1 of Wahba (1983): under the prior of its Section 2 the posterior
# covariance matrix of the fitted values is cov(g_hat | Y) = sigma^2 A(lambda),
# so the interval at a design point uses the DIAGONAL of the influence matrix:
#   g_hat(t_i) +- z * sigma_hat(lambda) * sqrt(a_ii(lambda)),
# with sigma_hat^2 = RSS(lambda) / n(1 - a(lambda)), n(1 - a) = Tr(I - A), and
# lambda chosen by GCV, eq. (2.16):
#   V(lambda) = n^-1 ||(I - A)y||^2 / [n^-1 Tr(I - A)]^2.
#
# Coverage is ACROSS THE FUNCTION -- the fraction of the n true g(t_i) covered
# -- and is not a pointwise coverage probability. The paper is explicit on this.
#
# References:
# Wahba, G. (1983) "Bayesian 'confidence intervals' for the cross-validated
# smoothing spline", Journal of the Royal Statistical Society Series B
# (Methodological) 45(1), 133-150, doi:10.1111/j.2517-6161.1983.tb01239.x.
# Craven, P. and Wahba, G. (1979) "Smoothing noisy data with spline functions",
# Numerische Mathematik 31(4), 377-403, doi:10.1007/BF01404567.
# Green, P. J. and Silverman, B. W. (1994) Nonparametric Regression and
# Generalized Linear Models, Chapman and Hall, ISBN 978-0-412-30040-0, Sec 2.1.2.

# banded Q (n x n-2) and R (n-2 x n-2) of the natural cubic spline
#' Banded Q (n x n-2) and R (n-2 x n-2) of the natural cubic spline
#'
#' A step of the funBand_native implementation. Called by \code{.funBand_roughness}.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param x A vector; its length is taken.
#' @return A list with \code{Q}, \code{R}.
#' @export
.funBand_qr_bands <- function(x) {
  n <- length(x)
  if (n < 4L) {
    stop(sprintf(paste0("funBand: a cubic smoothing spline needs at least ",
                        "four distinct design points, got %d"), n))
  }
  h <- diff(x)
  if (any(h <= 0)) {
    stop("funBand: the design points must be strictly increasing and distinct")
  }
  m <- n - 2L
  Q <- matrix(0.0, n, m)
  R <- matrix(0.0, m, m)
  for (j in seq_len(m)) {
    Q[j, j] <- 1.0 / h[j]
    Q[j + 1L, j] <- -1.0 / h[j] - 1.0 / h[j + 1L]
    Q[j + 2L, j] <- 1.0 / h[j + 1L]
    R[j, j] <- (h[j] + h[j + 1L]) / 3.0
    if (j + 1L <= m) {
      R[j, j + 1L] <- h[j + 1L] / 6.0
      R[j + 1L, j] <- h[j + 1L] / 6.0
    }
  }
  list(Q = Q, R = R)
}

# K = Q R^-1 Q', symmetric PSD with a two-dimensional null space
#' K = Q R^-1 Q\', symmetric PSD with a two-dimensional null space
#'
#' A step of the funBand_native implementation. Called by \code{morie_funBand_influence_matrix}.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param x Passed to \code{.funBand_qr_bands}.
#' @return A numeric value.
#' @export
.funBand_roughness <- function(x) {
  qr <- .funBand_qr_bands(x)
  Z <- t(solve(qr$R, t(qr$Q)))          # Z = Q R^-1
  K <- Z %*% t(qr$Q)
  0.5 * (K + t(K))
}

#' The influence matrix A(lambda) = (I + lambda K)^-1, computed spectrally
#'
#' K = U diag(d) U', so A = U diag(1/(1 + lambda d)) U'. Factorising
#' I + lambda K directly loses positive definiteness once lambda * ||K||
#' reaches about 1e12, which is exactly the limit the anchors probe.
#' @param x See Usage.
#' @param lam See Usage.
#' @export
morie_funBand_influence_matrix <- function(x, lam) {
  xs <- as.numeric(x)
  n <- length(xs)
  lm <- as.numeric(lam)
  if (lm < 0) stop("funBand: lambda must be non-negative")
  K <- .funBand_roughness(xs)
  e <- eigen(K, symmetric = TRUE)
  d <- e$values
  U <- e$vectors                        # eigenvectors in COLUMNS
  # K annihilates constants and linear terms exactly, so two eigenvalues are
  # zero by construction; the eigensolver returns them as O(1e-10) relative
  # residue, which at large lambda would damp the null space too and break
  # both the straight-line reproduction and the trace -> 2 limit.
  dmax <- if (length(d)) max(abs(d)) else 0.0
  tol <- n * 2.220446049250313e-16 * dmax
  w <- ifelse(abs(d) <= tol, 1.0, 1.0 / (1.0 + lm * pmax(d, 0.0)))
  A <- U %*% (w * t(U))
  0.5 * (A + t(A))
}

#' GCV, Wahba (1983) eq. (2.16)
#' @param y See Usage.
#' @param A See Usage.
#' @export
morie_funBand_gcv_score <- function(y, A) {
  n <- length(y)
  fit <- as.numeric(A %*% y)
  rss <- sum((y - fit) ^ 2)
  tr_ia <- n - sum(diag(A))
  if (abs(tr_ia) < 1e-12) return(Inf)
  (rss / n) / ((tr_ia / n) ^ 2)
}

#' Smoothing-spline fit with Wahba's Bayesian confidence intervals
#'
#' @param Y See Usage.
#' @param alpha See Usage.
#' @param x See Usage.
#' @param lam See Usage.
#' @param quantile See Usage.
#' @param truth See Usage.
#' @param n_lambda See Usage.
#' @param log_lambda_range See Usage.
#' @references
#' Wahba, G. (1983) Journal of the Royal Statistical Society Series B 45(1),
#' 133-150, doi:10.1111/j.2517-6161.1983.tb01239.x.
#' @export
morie_funBand <- function(Y, alpha = 0.05, x = NULL, lam = NULL,
                          quantile = "t", truth = NULL, n_lambda = 40L,
                          log_lambda_range = c(-8.0, 8.0)) {
  y <- as.numeric(Y)
  n <- length(y)
  if (n < 4L) {
    stop(sprintf("funBand: need at least four observations, got %d", n))
  }
  a <- as.numeric(alpha)
  if (!(a > 0 && a < 1)) {
    stop(sprintf("funBand: alpha must lie in (0, 1), got %g", a))
  }
  xs <- if (is.null(x)) seq_len(n) / n else as.numeric(x)
  if (length(xs) != n) {
    stop(sprintf("funBand: %d observations but %d design points", n, length(xs)))
  }
  if (!(quantile %in% c("t", "normal"))) {
    stop(sprintf("funBand: quantile must be 't' or 'normal', got '%s'",
                 as.character(quantile)))
  }

  if (is.null(lam)) {
    lo <- as.numeric(log_lambda_range[1])
    hi <- as.numeric(log_lambda_range[2])
    nl <- as.integer(n_lambda)
    grid <- 10 ^ (lo + (hi - lo) * (seq_len(nl) - 1L) / (nl - 1))
    best <- NULL
    for (lmv in grid) {
      A <- morie_funBand_influence_matrix(xs, lmv)
      v <- morie_funBand_gcv_score(y, A)
      if (is.null(best) || v < best$v) best <- list(v = v, lam = lmv, A = A)
    }
    gcv <- best$v; lam_used <- best$lam; A <- best$A
  } else {
    lam_used <- as.numeric(lam)
    A <- morie_funBand_influence_matrix(xs, lam_used)
    gcv <- morie_funBand_gcv_score(y, A)
  }

  fit <- as.numeric(A %*% y)
  resid <- y - fit
  rss <- sum(resid ^ 2)
  tr_a <- sum(diag(A))
  edf_err <- n - tr_a
  if (edf_err <= 0) {
    stop(paste0("funBand: the fit has no residual degrees of freedom; ",
                "lambda is too small for these data"))
  }
  sigma2 <- rss / edf_err
  sigma <- sqrt(sigma2)
  dg <- diag(A)

  z <- if (identical(quantile, "normal")) {
    stats::qnorm(1 - a / 2)
  } else {
    stats::qt(1 - a / 2, df = edf_err)
  }
  half <- z * sigma * sqrt(pmax(dg, 0.0))
  lower <- fit - half
  upper <- fit + half

  cover <- NULL
  if (!is.null(truth)) {
    g <- as.numeric(truth)
    if (length(g) != n) {
      stop(sprintf("funBand: %d observations but %d true values", n, length(g)))
    }
    cover <- sum(g >= lower & g <= upper) / n
  }

  list(
    estimate = fit,
    fitted = fit,
    lower = lower,
    upper = upper,
    half_width = half,
    residuals = resid,
    diag_A = dg,
    posterior_variance = sigma2 * dg,
    sigma2 = sigma2,
    sigma = sigma,
    lambda = lam_used,
    gcv = gcv,
    edf_signal = tr_a,
    edf_error = edf_err,
    rss = rss,
    multiplier = z,
    quantile = quantile,
    coverage = cover,
    alpha = a,
    n = n,
    x = xs,
    method = paste0("Bayesian confidence intervals for the cross-validated ",
                    "smoothing spline, Wahba (1983) Theorem 1 with GCV ",
                    "eq. (2.16)"),
    note = paste0("cov(g_hat | Y) = sigma^2 A(lambda), so the band uses the ",
                  "DIAGONAL of the influence matrix; coverage is measured ",
                  "ACROSS THE FUNCTION -- the fraction of the n true values ",
                  "covered -- and is not a pointwise coverage probability")
  )
}

#' @rdname morie_funBand
#' @export
morie_functional_band <- morie_funBand

#' morie_funBand_cheatsheet
#'
#' A step of the funBand_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @return A character value.
#' @export
morie_funBand_cheatsheet <- function() {
  paste0("funBand: smoothing-spline band. Theorem 1 of Wahba (1983): ",
         "cov(g_hat|Y) = sigma^2 A(lambda), so the interval at t_i is ",
         "g_hat +- z sigma_hat sqrt(a_ii) using the DIAGONAL of the ",
         "influence matrix. sigma_hat^2 = RSS/Tr(I-A); lambda by GCV ",
         "eq. (2.16). Coverage is ACROSS THE FUNCTION, not pointwise. ",
         "A reproduces straight lines exactly at every lambda.")
}
