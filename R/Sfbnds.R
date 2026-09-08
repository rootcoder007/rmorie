# SPDX-License-Identifier: AGPL-3.0-or-later
#' Balke-Pearl sharp bounds on the average causal effect under an instrument
#'
#' Sharp (tight) bounds on the average causal effect of D on Y when the
#' assignment Z is a valid instrument and compliance is imperfect.  All
#' three variables are binary.  Write
#' \code{p_yd.z = P(Y = y, D = d | Z = z)}, eight numbers with
#' \code{sum_{y,d} p_yd.z = 1} for each z.  Balke & Pearl solve the linear
#' program over the sixteen response-type probabilities by enumerating the
#' vertices of the dual constraint polytope, giving a closed form.  Their
#' eq. (4), p. 1173, is the lower bound, the maximum of
#' \code{p00.0 + p11.1 - 1}, \code{p00.1 + p11.1 - 1},
#' \code{p11.0 + p00.1 - 1}, \code{p00.0 + p11.0 - 1},
#' \code{2 p00.0 + p11.0 + p10.1 + p11.1 - 2},
#' \code{p00.0 + 2 p11.0 + p00.1 + p01.1 - 2},
#' \code{p10.0 + p11.0 + 2 p00.1 + p11.1 - 2},
#' \code{p00.0 + p01.0 + p00.1 + 2 p11.1 - 2};
#' their eq. (5) is the upper bound, the minimum of
#' \code{1 - p10.0 - p01.1}, \code{1 - p01.0 - p10.1},
#' \code{1 - p01.0 - p10.0}, \code{1 - p01.1 - p10.1},
#' \code{2 - 2 p01.0 - p10.0 - p10.1 - p11.1},
#' \code{2 - p01.0 - 2 p10.0 - p00.1 - p01.1},
#' \code{2 - p10.0 - p11.0 - 2 p01.1 - p10.1},
#' \code{2 - p00.0 - p01.0 - p01.1 - 2 p10.1}.
#'
#' The first four entries of each set are the Robins-Manski bounds; the last
#' four are what makes the Balke-Pearl interval strictly narrower.  The width
#' cannot exceed the rate of noncompliance \code{P(d1|z0) + P(d0|z1)}, which
#' is reported so the property can be checked.
#'
#' Both equations were read from a rendered image of p. 1173 of the JASA
#' printing; that PDF is a scan with no text layer, so no minus sign passed
#' through a text extractor.
#'
#' @param y Binary outcome, one entry per unit.
#' @param D Binary treatment actually received.
#' @param Z Binary instrument (assignment).
#' @return List with \code{estimate} (interval midpoint), \code{lower},
#'   \code{upper}, \code{width}, \code{lower_manski}, \code{upper_manski},
#'   \code{width_manski}, \code{noncompliance}, \code{itt},
#'   \code{compliance_gap}, \code{late}, \code{excludes_zero}, the eight
#'   \code{p} cells, \code{n}, \code{n_z0}, \code{n_z1}, \code{method}.
#' @references Balke, A. & Pearl, J. (1997). Bounds on treatment effects
#'   from studies with imperfect compliance. Journal of the American
#'   Statistical Association 92(439), 1171-1176, eqs. (4)-(5) p. 1173.
#'   \doi{10.1080/01621459.1997.10474074}
#' @export
#' @examples
#' set.seed(1)
#' Sfbnds(y = rbinom(40, 1, 0.5), D = rbinom(40, 1, 0.5), Z = rbinom(40, 1, 0.5))
Sfbnds <- function(y, D, Z) {
  chk <- function(v, nm) {
    x <- .s03vec(v)
    if (length(x) == 0L) stop(sprintf("sharp_bounds_balke_pearl: %s is empty", nm))
    if (any(x != 0 & x != 1)) stop(sprintf("sharp_bounds_balke_pearl: %s must be binary 0/1", nm))
    x
  }
  yy <- chk(y, "y")
  dd <- chk(D, "D")
  zz <- chk(Z, "Z")
  n <- length(yy)
  if (length(dd) != n || length(zz) != n)
    stop("sharp_bounds_balke_pearl: y, D and Z must have the same length")

  nz <- c(sum(zz == 0), sum(zz == 1))
  if (nz[1] == 0 || nz[2] == 0)
    stop("sharp_bounds_balke_pearl: both instrument arms must be non-empty")
  p <- function(a, b, cc) sum(yy == a & dd == b & zz == cc) / nz[cc + 1L]

  p00_0 <- p(0,0,0)
  p01_0 <- p(0,1,0)
  p10_0 <- p(1,0,0)
  p11_0 <- p(1,1,0)
  p00_1 <- p(0,0,1)
  p01_1 <- p(0,1,1)
  p10_1 <- p(1,0,1)
  p11_1 <- p(1,1,1)

  lower_terms <- c(
    p00_0 + p11_1 - 1,
    p00_1 + p11_1 - 1,
    p11_0 + p00_1 - 1,
    p00_0 + p11_0 - 1,
    2 * p00_0 + p11_0 + p10_1 + p11_1 - 2,
    p00_0 + 2 * p11_0 + p00_1 + p01_1 - 2,
    p10_0 + p11_0 + 2 * p00_1 + p11_1 - 2,
    p00_0 + p01_0 + p00_1 + 2 * p11_1 - 2)
  upper_terms <- c(
    1 - p10_0 - p01_1,
    1 - p01_0 - p10_1,
    1 - p01_0 - p10_0,
    1 - p01_1 - p10_1,
    2 - 2 * p01_0 - p10_0 - p10_1 - p11_1,
    2 - p01_0 - 2 * p10_0 - p00_1 - p01_1,
    2 - p10_0 - p11_0 - 2 * p01_1 - p10_1,
    2 - p00_0 - p01_0 - p01_1 - 2 * p10_1)
  lo <- max(lower_terms)
  up <- min(upper_terms)
  lo_rm <- max(lower_terms[1:4])
  up_rm <- min(upper_terms[1:4])

  noncompliance <- (p01_0 + p11_0) + (p00_1 + p10_1)
  itt <- (p10_1 + p11_1) - (p10_0 + p11_0)
  dz <- (p01_1 + p11_1) - (p01_0 + p11_0)

  .t1_result(estimate = 0.5 * (lo + up), lower = lo, upper = up,
             width = up - lo, lower_manski = lo_rm, upper_manski = up_rm,
             width_manski = up_rm - lo_rm, noncompliance = noncompliance,
             itt = itt, compliance_gap = dz,
             late = if (dz != 0) itt / dz else NA_real_,
             excludes_zero = if (lo > 0 || up < 0) 1 else 0,
             p00_0 = p00_0, p01_0 = p01_0, p10_0 = p10_0, p11_0 = p11_0,
             p00_1 = p00_1, p01_1 = p01_1, p10_1 = p10_1, p11_1 = p11_1,
             n = n, n_z0 = nz[1], n_z1 = nz[2],
             method = "Balke-Pearl sharp bounds on the ACE (Balke & Pearl 1997, eqs. 4-5)")
}
