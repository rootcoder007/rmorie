# SPDX-License-Identifier: AGPL-3.0-or-later
#' Proportional stratum allocation, n_h = n N_h / N
#'
#' Neyman, J. (1934), Journal of the Royal Statistical Society 97(4),
#' 558-625, JSTOR 2342192.  Page 567, read as a rendered page image, credits
#' this rule to Bowley: "Professor Bowley considered only the case when the
#' sizes, say m'_i, of the partial samples are proportional to the sizes of
#' corresponding strata" (the sentence runs on to p. 568).  Page 580, also
#' read as an image, gives what it costs: writing the variance of eq. (37)
#' as A + B - C of eq. (39), proportional allocation makes B = C
#' identically, so sigma^2 = A = ((M_0 - m_0) / m_0) sum_i M_i S_i^2, the
#' page's eq. (40) -- printed there with a misprint, "(M_0 - M_0)/m_0" for
#' "(M_0 - m_0)/m_0"; see Neyman() for the full note on that erratum.  The
#' optimum allocation reaches A - C instead, so proportional allocation is
#' worse by exactly C >= 0, and C = 0 precisely when every S_i is equal --
#' in which case the two allocations coincide.
#'
#' @param N population size M_0; must equal sum_h N_h when given.
#' @param Nh stratum sizes M_i, all positive.
#' @param n total number of units to allocate.
#' @return list: estimate, allocation, allocation_int, variance, A, fraction,
#'   N, n, method.  estimate is A = ((M_0 - n)/n) sum_i N_i, the variance of
#'   eq. (40) evaluated with every S_i = 1, which is the only variance
#'   available without S_h.
#' @keywords internal
#' @examples
#' Propal(600, c(100, 200, 300), 60)$allocation
#' @export
Propal <- function(N, Nh, n) {
  ck <- .s03allocCheck(Nh, NULL, n)
  M <- ck$M
  S <- ck$S
  m0 <- ck$m0
  H <- ck$H
  ab <- .s03allocABC(M, S, m0)
  if (!is.null(N)) {
    Ngiven <- as.numeric(N)
    if (is.na(Ngiven) || !(Ngiven > 0)) {
      stop("proportional_allocation: the population size N must be positive")
    }
    if (abs(Ngiven - ab$M0) > 1e-8 * max(1, abs(ab$M0))) {
      stop("proportional_allocation: N does not equal sum(Nh)")
    }
  }
  alloc <- numeric(H)
  for (i in seq_len(H)) alloc[i] <- m0 * M[i] / ab$M0
  var <- .s03allocVar(M, S, alloc)
  list(estimate = var, allocation = alloc,
       allocation_int = .s03allocInt(alloc, m0), variance = var, A = ab$A,
       fraction = m0 / ab$M0, N = ab$M0, n = H,
       method = paste0("Bowley proportional allocation n_h = n N_h / N; ",
                       "Neyman (1934) eq. (40) p. 580"))
}
