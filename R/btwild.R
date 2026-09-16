# SPDX-License-Identifier: AGPL-3.0-or-later
#' Wild bootstrap for OLS under heteroskedasticity (Mammen two-point)
#'
#' Mammen, E. (1993), "Bootstrap and Wild Bootstrap for High Dimensional
#' Linear Models", The Annals of Statistics 21(1), 255-285,
#' doi:10.1214/aos/1176349025 (verified against Crossref).
#'
#' The design and the residuals are both held fixed; only a scalar multiplier
#' is redrawn per observation, y*_i = x_i' beta_hat + r_i v_i, with v_i iid,
#' mean 0, variance 1, third moment 1.  Mammen's two-point law attains all
#' three: v = (1 - sqrt 5)/2 with probability (sqrt 5 + 1)/(2 sqrt 5), else
#' v = (1 + sqrt 5)/2.  That is the law the package's shared .s03mammen
#' helper encodes; here the point is drawn from the shared Lehmer stream
#' rather than a low-discrepancy sequence, because a single van der Corput
#' stream shared across the n positions of a replicate makes the multipliers
#' within a replicate deterministically dependent.  The Rademacher
#' alternative (+/-1 with probability 1/2) is available via
#' weights = "rademacher"; it has third moment 0 and so loses the skewness
#' correction, but is symmetric.
#'
#' Anchor, exact rather than asymptotic: because Var*(v) = 1 and the
#' multipliers are independent across i,
#' Var*(beta*) = (X'X)^-1 (sum_i r_i^2 x_i x_i') (X'X)^-1, precisely the HC0
#' heteroskedasticity-robust sandwich.  The wild bootstrap standard error
#' therefore targets the HC0 number, and var_hc0 reports that target computed
#' directly from the sandwich, never through the resampling loop.
#'
#' @param X the n x p design.
#' @param y the n responses.
#' @param B replicates.
#' @param seed seed for the shared deterministic stream.
#' @param alpha two-sided error rate.
#' @param weights "mammen" or "rademacher".
#' @return list: beta_b, beta_hat, se, lo, hi, var_hc0, v_mean, v_var, v_m3,
#'   n, p, B, estimate, method.
#' @keywords internal
#' @examples
#' X <- cbind(1, 1:12); y <- 2 + 0.5 * (1:12)
#' Btwild(X, y, B = 20)$var_hc0
#' @export
Btwild <- function(X, y, B = 200, seed = 1, alpha = 0.05, weights = "mammen") {
  if (!(weights %in% c("mammen", "rademacher")))
    stop("boot_wild_regression: weights must be 'mammen' or 'rademacher'")
  Xm <- .s03mat(X)
  yy <- .s03vec(y)
  n <- nrow(Xm)
  p <- ncol(Xm)
  if (n != length(yy)) stop("boot_wild_regression: X and y have different lengths")
  if (n <= p) stop("boot_wild_regression: need more rows than columns")
  if (as.integer(B) < 2L) stop("boot_wild_regression: need at least two replicates")
  a <- as.numeric(alpha)
  if (!(a > 0 && a < 1)) stop("boot_wild_regression: alpha must lie strictly between 0 and 1")
  bh <- .s03lstsq(Xm, yy)
  fit <- as.numeric(Xm %*% bh)
  res <- yy - fit
  XtXinv <- .btres_xtxinv(Xm, p)
  meat <- matrix(0, p, p)
  for (i in seq_len(n)) meat <- meat + (res[i]^2) * (Xm[i, ] %o% Xm[i, ])
  sw <- XtXinv %*% meat %*% XtXinv
  hc0 <- diag(sw)
  g <- .t1_lcg(seed)
  reps <- vector("list", as.integer(B))
  s1 <- 0
  s2 <- 0
  s3 <- 0
  N <- 0L
  for (b in seq_len(as.integer(B))) {
    ys <- numeric(n)
    for (i in seq_len(n)) {
      v <- .btwild_mult(g$unif(), weights)
      s1 <- s1 + v
      s2 <- s2 + v * v
      s3 <- s3 + v * v * v
      N <- N + 1L
      ys[i] <- fit[i] + res[i] * v
    }
    reps[[b]] <- .s03lstsq(Xm, ys)
  }
  se <- numeric(p)
  lo <- numeric(p)
  hi <- numeric(p)
  for (j in seq_len(p)) {
    col <- vapply(reps, function(r) r[j], 0)
    se[j] <- .s03sd(col, 1L)
    lo[j] <- .s03quantile7(col, a / 2)
    hi[j] <- .s03quantile7(col, 1 - a / 2)
  }
  vm <- s1 / N
  list(beta_b = reps, beta_hat = bh, se = se, lo = lo, hi = hi, var_hc0 = hc0,
       v_mean = vm, v_var = s2 / N - vm * vm, v_m3 = s3 / N,
       n = n, p = p, B = as.integer(B), estimate = bh[1],
       method = "Mammen (1993) Ann. Statist. 21(1):255-285, two-point multiplier")
}

#' @noRd
.btwild_mult <- function(u, kind) {
  r5 <- sqrt(5)
  if (identical(kind, "rademacher")) return(if (u < 0.5) 1 else -1)
  if (u < (r5 + 1) / (2 * r5)) (1 - r5) / 2 else (1 + r5) / 2
}
