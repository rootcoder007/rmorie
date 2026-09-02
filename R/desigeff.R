# SPDX-License-Identifier: AGPL-3.0-or-later

#' Design effect of unequal weighting and clustering
#'
#' Formula: DEFF = Var_complex / Var_SRS
#'
#' Decomposed the way Kish does it, into the unequal-weighting effect
#' DEFF_w = n sum w_i^2 / (sum w_i)^2 and the clustering effect
#' DEFF_c = 1 + (m0 - 1) rho, where rho is the one-way ANOVA intraclass
#' correlation and m0 the Kish average cluster size.  The reported DEFF
#' is their product.  Equal weights give DEFF_w = 1 exactly, and
#' singleton clusters give DEFF_c = 1 exactly.
#'
#' @param y Survey variable, length n.
#' @param weights Sampling weights, or NULL for equal weights.
#' @param cluster Cluster (PSU) identifier per unit, or NULL.
#' @return List with \code{estimate} (DEFF), \code{deff_w},
#'   \code{deff_c}, \code{rho}, \code{m0}, \code{n_eff}, \code{n},
#'   \code{method}.
#' @references Kish (1965), Survey Sampling, Wiley, sections 8.2 and 5.4.
#' @export
#' @examples
#' V <- c(1, 2, 3, 4, 5, 6, 7, 8)
#' Desigeff(V)
Desigeff <- function(y, weights = NULL, cluster = NULL) {
  y <- .s03vec(y)
  n <- length(y)
  if (n == 0L) stop("empty input: y has no observations")
  w <- if (is.null(weights)) rep(1, n) else .s03vec(weights)
  if (length(w) != n) stop("y and weights must have the same length")
  if (any(w < 0)) stop("weights must be non-negative")
  sw <- sum(w)
  if (sw <= 0) stop("weights must not sum to zero")
  deff_w <- n * sum(w * w) / (sw * sw)
  ids <- if (is.null(cluster)) seq_len(n) else cluster
  if (length(ids) != n) stop("y and cluster must have the same length")
  keys <- unique(ids)
  groups <- lapply(keys, function(k) y[ids == k])
  a <- length(groups)
  sizes <- vapply(groups, length, 0L)
  gm <- sum(y) / n
  ssb <- 0
  for (j in seq_len(a)) ssb <- ssb + sizes[j] * (sum(groups[[j]]) / sizes[j] - gm)^2
  ssw <- 0
  for (g in groups) ssw <- ssw + sum((g - sum(g) / length(g))^2)
  if (a > 1L && n > a) {
    msb <- ssb / (a - 1)
    msw <- ssw / (n - a)
    m0 <- (n - sum(sizes * sizes) / n) / (a - 1)
    denom <- msb + (m0 - 1) * msw
    rho <- if (denom != 0) (msb - msw) / denom else 0
  } else {
    m0 <- n / a
    rho <- 0
  }
  deff_c <- 1 + (m0 - 1) * rho
  deff <- deff_w * deff_c
  .t1_result(estimate = deff, deff_w = deff_w, deff_c = deff_c, rho = rho,
             m0 = m0, n_eff = if (deff > 0) n / deff else NaN, n = n,
             method = "Kish design effect: unequal weighting x clustering")
}
