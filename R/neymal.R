# SPDX-License-Identifier: AGPL-3.0-or-later
#' Neyman optimal allocation, stated as n_h proportional to N_h S_h
#'
#' Same source and same equations as Neyman() -- Neyman, J. (1934), Journal
#' of the Royal Statistical Society 97(4), 558-625, JSTOR 2342192, p. 580
#' eq. (39) and eq. (41), read as a rendered page image.  This entry point
#' differs only in taking the population size N = M_0 as its first argument
#' instead of the stratum means, and in checking it against sum_h N_h, which
#' eq. (39) requires to be the same number.
#'
#' The optimum allocation, the vanishing of the term B of eq. (39) at it,
#' and the erratum in the printed eq. (40) are all documented on Neyman();
#' that function is the primary write-up.
#'
#' @param N population size M_0.  When given it must equal sum_h N_h to
#'   within 1e-8 relative, since eq. (39) uses the one number in both roles;
#'   NULL skips the check and uses sum_h N_h.
#' @param Nh stratum sizes M_i, all positive.
#' @param Sh within-stratum standard deviations S_i, divisor M_i - 1.
#' @param n total number of units to allocate.
#' @return list: estimate, allocation, allocation_int, variance, A, B, C, N,
#'   n, method.
#' @keywords internal
#' @examples
#' Neymal(600, c(100, 200, 300), c(1, 2, 3), 60)$allocation
#' @export
Neymal <- function(N, Nh, Sh, n) {
  ck <- .s03allocCheck(Nh, Sh, n)
  M <- ck$M
  S <- ck$S
  m0 <- ck$m0
  H <- ck$H
  ab <- .s03allocABC(M, S, m0)
  if (!is.null(N)) {
    Ngiven <- as.numeric(N)
    if (is.na(Ngiven) || !(Ngiven > 0)) {
      stop("neyman_allocation: the population size N must be positive")
    }
    if (abs(Ngiven - ab$M0) > 1e-8 * max(1, abs(ab$M0))) {
      stop("neyman_allocation: N does not equal sum(Nh)")
    }
  }
  if (!(ab$T > 0)) {
    stop("neyman_allocation: sum(Nh Sh) must be positive; every stratum has Sh = 0")
  }
  alloc <- numeric(H)
  for (i in seq_len(H)) alloc[i] <- m0 * M[i] * S[i] / ab$T
  var <- .s03allocVar(M, S, alloc)
  B <- 0
  for (i in seq_len(H)) {
    if (alloc[i] > 0) {
      d <- M[i] * S[i] / alloc[i] - ab$T / m0
      B <- B + alloc[i] * d * d
    }
  }
  list(estimate = var, allocation = alloc,
       allocation_int = .s03allocInt(alloc, m0), variance = var, A = ab$A,
       B = B, C = ab$C, N = ab$M0, n = H,
       method = paste0("Neyman (1934) optimum allocation n_h ~ N_h S_h, ",
                       "eq. (39) p. 580"))
}
