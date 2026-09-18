# SPDX-License-Identifier: AGPL-3.0-or-later

#' Nakagawa-Schielzeth R-squared for a mixed model
#'
#' Formula: R2_c = (sigma2_f + sum sigma2_l) / (sigma2_f + sum sigma2_l + sigma2_e)
#'
#' The marginal R2 drops the random-effect variance from the numerator
#' and keeps it in the denominator, so it measures what the fixed
#' effects alone explain; the conditional R2 credits both.  A residual
#' variance of exactly zero therefore forces R2_c = 1, the degenerate
#' case used to check the algebra.  Variance components come from a
#' one-way random-intercept fit by moments.
#'
#' @param y Response.
#' @param X Fixed-effect design, or NULL.
#' @param Z Ignored; the random effect is the intercept of \code{cluster}.
#' @param cluster Grouping factor, or NULL.
#' @return List with \code{estimate}, \code{r2_marginal},
#'   \code{r2_conditional}, \code{var_fixed}, \code{var_random},
#'   \code{var_resid}, \code{icc}, \code{n}, \code{n_groups},
#'   \code{method}.
#' @references Nakagawa & Schielzeth (2013), Methods Ecol. Evol.
#'   4(2):133-142.
#' @export
#' @examples
#' V <- c(1, 2, 3, 4, 5, 6, 7, 8)
#' Ccngg(V)
Ccngg <- function(y, X = NULL, Z = NULL, cluster = NULL) {
  yv <- .s03vec(y)
  n <- length(yv)
  if (n < 3L) stop("need at least three observations")
  D <- if (is.null(X)) matrix(1, n, 1L) else {
    Xm <- .s03mat(X)
    if (nrow(Xm) != n) stop("y and X must have the same number of rows")
    cbind(rep(1, n), Xm)
  }
  D <- matrix(as.numeric(D), n)
  p <- ncol(D)
  if (n <= p) stop("need more observations than fixed effects")
  beta <- .s03cholsolve(.s03crossprod(D), .s03matvec(t(D), yv))
  fit <- .s03matvec(D, beta)
  mf <- sum(fit) / n
  var_f <- sum((fit - mf)^2) / (n - 1)
  res <- yv - fit
  if (is.null(cluster)) {
    var_r <- 0
    a <- 1L
    var_e <- sum(res * res) / (n - p)
  } else {
    ids <- cluster
    if (length(ids) != n) stop("y and cluster must have the same length")
    keys <- unique(ids)
    a <- length(keys)
    groups <- lapply(keys, function(k) res[ids == k])
    sizes <- vapply(groups, length, 0L)
    gm <- sum(res) / n
    ssb <- 0
    for (j in seq_len(a))
      ssb <- ssb + sizes[j] * (sum(groups[[j]]) / sizes[j] - gm)^2
    ssw <- 0
    for (g in groups) ssw <- ssw + sum((g - sum(g) / length(g))^2)
    if (a > 1L && n > a) {
      msb <- ssb / (a - 1)
      msw <- ssw / (n - a)
      m0 <- (n - sum(sizes * sizes) / n) / (a - 1)
      var_e <- msw
      var_r <- max((msb - msw) / m0, 0)
    } else {
      var_e <- ssw / max(n - a, 1L)
      var_r <- 0
    }
  }
  tot <- var_f + var_r + var_e
  if (tot <= 0) stop("total variance is zero; R-squared is undefined")
  .t1_result(estimate = (var_f + var_r) / tot, r2_marginal = var_f / tot,
             r2_conditional = (var_f + var_r) / tot, var_fixed = var_f,
             var_random = var_r, var_resid = var_e,
             icc = if ((var_r + var_e) > 0) var_r / (var_r + var_e) else NaN,
             n = n, n_groups = a,
             method = "Nakagawa-Schielzeth marginal and conditional R-squared")
}
