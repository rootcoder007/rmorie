# SPDX-License-Identifier: AGPL-3.0-or-later
#' Entropic transport cost with its own bias subtracted off
#'
#' The regularised cost \code{OT_eps(mu, mu)} is not zero, so
#' \code{OT_eps} alone is not a divergence and a generative model trained
#' on it drifts towards a blurred target. Subtracting half of each
#' self-cost fixes that: the result is zero exactly when the two measures
#' coincide.
#'
#' Formula: \code{S_eps(mu,nu) = OT_eps(mu,nu) - 0.5 (OT_eps(mu,mu) +
#' OT_eps(nu,nu))} with \code{OT_eps(mu,nu) = <T*,C> + eps KL(T* | mu x
#' nu)} -- Genevay, Peyre and Cuturi (2018) eq. (3)-(4).
#'
#' @param a,b The two histograms.
#' @param Cab Cross cost, n by m.
#' @param Caa Cost of \code{a} against itself, n by n.
#' @param Cbb Cost of \code{b} against itself, m by m.
#' @param epsilon Regularisation strength, positive.
#' @param max_iter Sinkhorn sweeps.
#' @return List with \code{S_eps}, \code{OT_ab}, \code{OT_aa},
#'   \code{OT_bb}, \code{n}, \code{m}.
#' @references Genevay, A., Peyre, G. and Cuturi, M. (2018). Proceedings
#'   of Machine Learning Research 84:1608-1617 (AISTATS).
#' @export
Otdiv <- function(a, b, Cab, Caa, Cbb, epsilon, max_iter = 200) {
  aa <- .ot_hist(a)
  bb <- .ot_hist(b)
  Cx <- as.matrix(Cab)
  Ca <- as.matrix(Caa)
  Cb <- as.matrix(Cbb)
  n <- length(aa)
  m <- length(bb)
  if (nrow(Cx) != n || ncol(Cx) != m)
    stop("cross cost does not match the marginals")
  if (nrow(Ca) != n || nrow(Cb) != m)
    stop("self costs do not match the marginals")
  eps <- as.numeric(epsilon)
  oteps <- function(p, q, Cm) {
    s <- .ot_sinkhorn(p, q, Cm, eps, max_iter)
    sum(s$T * Cm) + eps * .ot_kl(s$T, outer(p, q))
  }
  ab <- oteps(aa, bb, Cx)
  a2 <- oteps(aa, aa, Ca)
  b2 <- oteps(bb, bb, Cb)
  .t1_result(S_eps = ab - 0.5 * (a2 + b2), OT_ab = ab, OT_aa = a2,
             OT_bb = b2, n = n, m = m, method = "Sinkhorn divergence")
}
