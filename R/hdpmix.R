# SPDX-License-Identifier: AGPL-3.0-or-later
#' Hierarchical Dirichlet process
#'
#' Teh, Jordan, Beal and Blei (2006), Hierarchical Dirichlet processes,
#' JASA 101(476), 1566-1581 (FETCHED as PDF from the author's page).
#' Equation (2): G_0 | gamma, H ~ DP(gamma, H) and G_j | alpha_0, G_0 ~
#' DP(alpha_0, G_0).  Equation (19): beta | gamma ~ GEM(gamma), pi_j |
#' alpha_0, beta ~ DP(alpha_0, beta), z_ji | pi_j ~ pi_j, with GEM the
#' stick-breaking law of eqs. (5)-(6).  G_0 being DISCRETE is what lets
#' the groups share atoms at all, which is the paper's central point.
#'
#' Determinism: beta from the exact Beta quantile at low-discrepancy
#' points; pi_j from the group's Dirichlet posterior mean given beta, not
#' a draw.
#'
#' @param y atom index (zero-based) per observation.
#' @param groups group label per observation.
#' @param gamma,alpha top- and group-level concentrations.
#' @param truncation number of atoms retained.
#' @return list: estimate, beta, pi, counts, shared, group_ids, method.
#' @keywords internal
#' @examples
#' Hdpmix(c(0, 1, 0, 2, 1, 1), c("a", "a", "a", "b", "b", "b"))$shared
#' @export
Hdpmix <- function(y, groups = NULL, gamma = 1, alpha = 1, truncation = 6) {
  z <- as.integer(.s03vec(y))
  g <- as.character(if (!is.null(groups)) groups else rep(0, length(z)))
  ids <- character(0)
  for (cc in g) if (!(cc %in% ids)) ids <- c(ids, cc)
  K <- as.integer(truncation)
  beta <- Stickw(gamma, K)$pi
  tot <- 0
  for (x in beta) tot <- tot + x
  beta <- if (tot > 0) beta / tot else rep(1 / K, K)
  counts <- vector("list", length(ids)); pi_ <- vector("list", length(ids))
  for (ci in seq_along(ids)) {
    row <- numeric(K)
    for (i in seq_along(z)) {
      if (g[i] == ids[ci] && z[i] >= 0L && z[i] < K) row[z[i] + 1L] <- row[z[i] + 1L] + 1
    }
    counts[[ci]] <- row
    nj <- 0
    for (x in row) nj <- nj + x
    pi_[[ci]] <- (as.numeric(alpha) * beta + row) / (as.numeric(alpha) + nj)
  }
  shared <- 0L
  for (t in seq_len(K)) {
    used <- 0L
    for (row in counts) if (row[t] > 0) used <- used + 1L
    if (used > 1L) shared <- shared + 1L
  }
  list(estimate = if (K) beta[1] else NaN, beta = beta, pi = pi_,
       counts = counts, shared = shared, group_ids = ids,
       method = "HDP: beta ~ GEM(gamma), pi_j ~ DP(alpha_0, beta) at its posterior mean (Teh et al. 2006, eqs. 2 and 19)")
}
