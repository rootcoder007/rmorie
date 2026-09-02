# SPDX-License-Identifier: AGPL-3.0-or-later
#' Balke-Pearl sharp ATE bounds by linear programming
#'
#' With binary outcome, treatment and instrument every unit belongs to one
#' of sixteen response types: how its treatment responds to the instrument,
#' crossed with how its outcome responds to treatment. The eight observed
#' conditional probabilities are linear in the type shares, and the ATE is
#' linear in them too, so the sharp bounds are the optimum of a linear
#' program over the simplex. Monotonicity is NOT imposed: defiers keep
#' their own share, and the program is what makes the bounds sharp rather
#' than merely valid.
#'
#' Formula: \code{min / max sum q_jk \[y_k(1) - y_k(0)\]} subject to
#' \code{sum over types consistent with (a, b, z) of q = P(y = a, D = b |
#' Z = z)} for all \code{a, b, z}, and \code{q >= 0}.
#'
#' @param y Binary outcome, coded 0/1.
#' @param D Binary treatment, coded 0/1.
#' @param Z Binary instrument, coded 0/1.
#' @param moment_eqs Optional (m, 17) matrix of extra linear equality
#'   constraints on the sixteen type shares: sixteen coefficients then the
#'   right-hand side. Default \code{NULL}.
#' @return List with \code{lower}, \code{upper}, \code{width},
#'   \code{estimate}, \code{feasible}, \code{n_constraints}, \code{n}.
#' @references Balke, A. and Pearl, J. (1997). Bounds on treatment effects
#'   from studies with imperfect compliance. Journal of the American
#'   Statistical Association 92(439), 1171-1176.
#'   \doi{10.1080/01621459.1997.10474074}. The response-type
#'   parameterisation used here is the one described in Molinari, F.
#'   (2021), Handbook of Econometrics 7A (arXiv:2004.11751 p. 19 and
#'   note 10).
#' @export
#' @examples
#' set.seed(1)
#' Bndlpm(y = rbinom(40, 1, 0.5), D = rbinom(40, 1, 0.5), Z = rbinom(40, 1, 0.5))
Bndlpm <- function(y, D, Z, moment_eqs = NULL) {
  yv <- as.numeric(unlist(y))
  dv <- as.numeric(unlist(D))
  zv <- as.numeric(unlist(Z))
  n <- length(yv)
  if (n == 0L) stop("Bndlpm: y is empty")
  if (length(dv) != n || length(zv) != n)
    stop("Bndlpm: y, D and Z must have the same length")
  if (any(!(c(yv, dv, zv) %in% c(0, 1))))
    stop("Bndlpm: y, D and Z must be coded 0/1")
  nz <- c(sum(zv == 0), sum(zv == 1))
  if (nz[1] == 0L || nz[2] == 0L)
    stop("Bndlpm: the instrument takes only one value")
  DZ <- rbind(c(0, 0), c(0, 1), c(1, 0), c(1, 1))
  YK <- rbind(c(0, 0), c(0, 1), c(1, 0), c(1, 1))
  A <- matrix(0, 0L, 16L)
  bvec <- numeric(0)
  for (z in 0:1) for (b in 0:1) for (a in 0:1) {
    row <- numeric(16)
    for (j in 1:4) {
      if (DZ[j, z + 1L] != b) next
      for (k in 1:4) if (YK[k, b + 1L] == a) row[(j - 1L) * 4L + k] <- 1
    }
    A <- rbind(A, row)
    bvec <- c(bvec, sum(zv == z & dv == b & yv == a) / nz[z + 1L])
  }
  if (!is.null(moment_eqs)) {
    M <- as.matrix(moment_eqs)
    if (ncol(M) != 17L)
      stop("Bndlpm: each extra constraint needs 17 entries")
    A <- rbind(A, M[, 1:16, drop = FALSE])
    bvec <- c(bvec, as.numeric(M[, 17]))
  }
  cvec <- numeric(16)
  for (j in 1:4) for (k in 1:4)
    cvec[(j - 1L) * 4L + k] <- YK[k, 2] - YK[k, 1]
  r <- morie_bnd_lp(cvec, A_eq = A, b_eq = bvec,
                    bounds = rep(list(c(0, 1)), 16L))
  .t1_result(lower = r$lower, upper = r$upper,
             width = r$upper - r$lower,
             estimate = 0.5 * (r$lower + r$upper),
             feasible = if (isTRUE(r$feasible)) 1 else 0,
             n_constraints = nrow(A), n = n,
             method = "Linear programming method for bounds")
}
