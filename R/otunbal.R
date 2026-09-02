# SPDX-License-Identifier: AGPL-3.0-or-later
#' Transport between histograms of unequal mass
#'
#' Hard marginal constraints force every unit of mass to be matched, which
#' is exactly wrong when one measure is a noisy or truncated view of the
#' other -- an outlier then drags a long-distance flow. Replacing the
#' constraints with KL penalties lets mass be created and destroyed at
#' price \code{lam}; \code{lam -> Inf} recovers the balanced problem.
#'
#' Formula: \code{min_T <T,C> + eps H(T) + lam KL(T 1 | a) + lam KL(T' 1 |
#' b)}, solved by \code{u <- (a/(K v))^(lam/(lam+eps))},
#' \code{v <- (b/(K' u))^(lam/(lam+eps))} -- Peyre and Cuturi (2019)
#' eq. (10.8)-(10.9), p. 163; Chizat et al. (2018).
#'
#' @param a,b Marginals; the totals need not agree.
#' @param C Ground cost, n by m.
#' @param epsilon Entropic strength, positive.
#' @param lam Marginal-relaxation strength, positive.
#' @param max_iter Scaling sweeps.
#' @return List with \code{T}, \code{cost}, \code{mass}, \code{mass_a},
#'   \code{mass_b}, \code{n}, \code{m}.
#' @references Chizat, L., Peyre, G., Schmitzer, B. and Vialard, F.-X.
#'   (2018). Mathematics of Computation 87(314):2563-2609.
#'   \doi{10.1090/mcom/3303}.
#' @export
Otunbal <- function(a, b, C, epsilon, lam, max_iter = 200) {
  aa <- .ot_hist(a)
  bb <- .ot_hist(b)
  Cm <- as.matrix(C)
  n <- length(aa)
  m <- length(bb)
  if (nrow(Cm) != n || ncol(Cm) != m)
    stop("cost matrix does not match the marginals")
  T <- .ot_sinkhorn_unbalanced(aa, bb, Cm, as.numeric(epsilon),
                               as.numeric(lam), max_iter)
  .t1_result(T = T, cost = sum(T * Cm), mass = sum(T),
             mass_a = sum(aa), mass_b = sum(bb), n = n, m = m,
             method = "Unbalanced optimal transport")
}
