# SPDX-License-Identifier: AGPL-3.0-or-later

#' Structural violation losses of AlphaFold
#'
#' Supplement section 1.9.11, equations (44)-(47) of Jumper et al. (2021),
#' p. 40.  Three flat-bottom penalties that charge nothing while the
#' geometry stays within tolerance and grow linearly beyond it: bond
#' lengths against literature values, bond angles through the cosine of the
#' angle, and a one-sided clash term that penalises only distances that are
#' too short.
#'
#' Equations (44) and (45) average over bonds and angles while (46) sums
#' over non-bonded pairs; that asymmetry is in the published text and is
#' reproduced rather than tidied away.
#'
#' @param blen,blen_lit,blen_sigma Predicted bond lengths, literature
#'   values and standard deviations (equation 44).
#' @param cosang,cosang_lit,cosang_sigma Cosines of predicted and
#'   literature bond angles and their standard deviations (equation 45).
#' @param dnb,dnb_lit Non-bonded distances and clashing distances (eq. 46).
#' @param factor Multiplier on the literature standard deviation, 12 in
#'   the spec.
#' @param clash_tol Clash tolerance in angstrom, 1.5 in the spec.
#' @return A list with \code{bondlength}, \code{bondangle}, \code{clash},
#'   their sum \code{estimate} (equation 47) and \code{method}.
#' @references Jumper et al (2021) Nature 596:583-589, Suppl. eq. (44)-(47)
#' @examples
#' rmorie:::Alfviol()
Alfviol <- function(blen = NULL, blen_lit = NULL, blen_sigma = NULL,
                    cosang = NULL, cosang_lit = NULL, cosang_sigma = NULL,
                    dnb = NULL, dnb_lit = NULL, factor = 12, clash_tol = 1.5) {
  flat <- function(x, tol) pmax(abs(x) - tol, 0)
  lb <- if (is.null(blen)) 0 else
    mean(flat(blen - blen_lit, factor * blen_sigma))
  la <- if (is.null(cosang)) 0 else
    mean(flat(cosang - cosang_lit, factor * cosang_sigma))
  lc <- if (is.null(dnb)) 0 else
    sum(pmax(dnb_lit - clash_tol - dnb, 0))
  list(bondlength = lb, bondangle = la, clash = lc, estimate = lb + la + lc,
       method = "AlphaFold structural violation loss")
}
