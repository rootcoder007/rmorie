# SPDX-License-Identifier: AGPL-3.0-or-later
#' Neutral-to-the-right posterior consistency
#'
#' Pi_n(F : d(F, F0) > eps | data) tends to zero under a KL-type support
#' condition on the NTR prior.  Consistency under CENSORING is the point:
#' the observed information about F is only partial, and the theorem says
#' the prior still washes out.  The demonstration runs the Bayesian
#' product-limit estimator under light censoring and watches its error at
#' t = 1 fall with n.
#'
#' Formula: S(1) = prod over events t_j <= 1 of
#'   (R_j - 1 + 2 exp(-t_j)) / (R_j + 2 exp(-t_j)); target exp(-1).
#'
#' @param ns Increasing vector of sample sizes.
#' @param seed Seed for the deterministic draws.
#' @return List with \code{estimate} (error at the largest n),
#'   \code{err_by_n}, \code{improving}, \code{method}.
#' @references Ghosal & van der Vaart (2017), Fundamentals of
#'   Nonparametric Bayesian Inference, CUP, section 13.4.1.
#' @export
#' @examples
#' Ghosalntrconsist()
Ghosalntrconsist <- function(ns = c(100, 800, 6400), seed = 42) {
  ns <- as.integer(ns)
  if (length(ns) < 2L) stop("ns must have at least two sample sizes")
  if (any(ns < 1L)) stop("every n must be positive")
  e <- .ghc_rng(seed)
  errs <- numeric(length(ns))
  for (k in seq_along(ns)) {
    n <- ns[k]
    # the Python arm draws one survival uniform and one censoring
    # uniform per observation, interleaved, so take 2n and de-interleave
    uu <- .ghc_unif(e, 2L * n)
    x <- -log(pmax(uu[seq(1L, 2L * n, by = 2L)], 1e-12))
    cens <- 3 * uu[seq(2L, 2L * n, by = 2L)]
    times <- pmin(x, cens)
    events <- as.numeric(x <= cens)
    ord <- order(times)
    surv <- 1
    at_risk <- n
    for (i in ord) {
      if (times[i] > 1) break
      if (events[i] > 0) {
        s0 <- 2 * exp(-times[i])
        surv <- surv * (at_risk - 1 + s0) / (at_risk + s0)
      }
      at_risk <- at_risk - 1
    }
    errs[k] <- abs(surv - exp(-1))
  }
  .t1_result(estimate = errs[length(errs)], err_by_n = errs,
             improving = errs[length(errs)] < errs[1],
             method = "NTR consistency (GvdV 2017 sec. 13.4.1)")
}
