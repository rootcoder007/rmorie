# morie native arm -- acigls
# Inverse-probability-weighted GLS with a cluster-robust variance.
#
# The weights fix the point estimate; the sandwich fixes the standard
# error. Neither substitutes for the other, so both are returned
# alongside the naive standard error to make each correction visible.
# Consistent in the number of CLUSTERS, not observations.
#
# Liang, K.-Y. & Zeger, S. L. (1986) Biometrika 73(1), 13-22,
# doi:10.1093/biomet/73.1.13.

#' morie_acigls
#'
#' Part of the acigls_native implementation; see the file header for the
#' source it follows.
#'
#' @param y See Usage.
#' @param A See Usage.
#' @param H See Usage.
#' @param cluster See Usage.
#' @param small_sample Defaults to \code{TRUE}.
#' @return A list with \code{estimate}, \code{coefficients}, \code{std_errors}, \code{naive_std_errors}, \code{vcov}, \code{residuals}, \code{n}, \code{n_clusters}, \code{n_coefficients}, \code{sum_weights}, \code{finite_sample_correction}, \code{inflation}, \code{method}.
#' @export
morie_acigls <- function(y, A, H, cluster, small_sample = TRUE) {
  y <- as.numeric(y)
  n <- length(y)
  if (n < 2L) stop("acigls: need at least two observations")

  A <- as.matrix(A)
  if (nrow(A) != n) {
    stop(sprintf("acigls: A has %d rows but y has %d entries",
                 nrow(A), n))
  }
  p <- ncol(A)
  if (p < 1L) stop("acigls: A has no columns")

  w <- as.numeric(H)
  if (length(w) != n) {
    stop(sprintf("acigls: %d weights but %d observations",
                 length(w), n))
  }
  if (any(w <= 0)) {
    stop(paste0("acigls: every weight must be positive; an ",
                "inverse-probability weight cannot be zero or ",
                "negative"))
  }

  cl <- as.character(cluster)
  if (length(cl) != n) {
    stop(sprintf("acigls: %d cluster labels but %d observations",
                 length(cl), n))
  }
  keys <- unique(cl)
  G <- length(keys)
  if (G < 2L) {
    stop(paste0("acigls: the cluster-robust variance needs at least ",
                "two clusters; with one there is no between-cluster ",
                "information"))
  }
  if (n <= p) {
    stop(sprintf("acigls: %d observations cannot support %d coefficients",
                 n, p))
  }

  XtWX <- crossprod(A, A * w)
  XtWy <- crossprod(A, w * y)
  beta <- tryCatch(as.numeric(solve(XtWX, XtWy)),
                   error = function(e)
                     stop(paste0("acigls: A'WA is singular -- the design ",
                                 "has collinear columns, or a weight has ",
                                 "removed a column's variation")))
  resid <- as.numeric(y - A %*% beta)

  bread <- solve(XtWX)
  meat <- matrix(0, p, p)
  for (k in keys) {
    idx <- which(cl == k)
    u <- as.numeric(crossprod(A[idx, , drop = FALSE],
                              w[idx] * resid[idx]))
    meat <- meat + tcrossprod(u)
  }
  corr <- if (isTRUE(small_sample)) {
    (G / (G - 1)) * ((n - 1) / (n - p))
  } else 1
  V <- corr * (bread %*% meat %*% bread)
  dv <- diag(V)
  se <- ifelse(dv > 0, sqrt(dv), NA_real_)

  s2 <- sum(w * resid^2) / (n - p)
  db <- s2 * diag(bread)
  naive <- ifelse(db > 0, sqrt(db), NA_real_)

  list(
    estimate = beta,
    coefficients = beta,
    std_errors = as.numeric(se),
    naive_std_errors = as.numeric(naive),
    vcov = V,
    residuals = resid,
    n = n,
    n_clusters = G,
    n_coefficients = p,
    sum_weights = sum(w),
    finite_sample_correction = corr,
    inflation = as.numeric(se / naive),
    method = paste0("IPW-GLS with a cluster-robust sandwich variance ",
                    "(Liang & Zeger 1986)")
  )
}
