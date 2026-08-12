# Cross-population extended haplotype homozygosity (XP-EHH).
# Source: Sabeti, P. C. et al. (2007), Genome-wide detection and
# characterization of positive selection in human populations, Nature
# 449, 913-918.  XP-EHH compares the SAME core site between two
# populations rather than two alleles within one: the statistic is
# ln(I_A / I_B), where each I is the trapezoid integral of the
# site-wise EHH computed over ALL chromosomes of that population.
# Because it does not condition on an allele, XP-EHH retains power
# where a sweep has gone to (or near) fixation and iHS has lost it.
#
# Native implementation mirroring Python morie.fn.xpehh1 exactly.

#' Cross-population EHH (XP-EHH)
#'
#' \eqn{\ln(I_A / I_B)} for a core site shared by two populations,
#' where \eqn{I} integrates the all-chromosome site-wise EHH outward
#' from the core (Sabeti et al. 2007).  Large positive values indicate
#' unusually long haplotypes in population A relative to B.
#'
#' @param hapA,hapB Haplotype matrices for the two populations, over
#'   the same SNPs, coded 0/1.
#' @param core Core site index, 0-based.
#' @param positions Optional physical positions.
#' @param min_ehh EHH threshold at which integration stops.
#' @param standardize Optional \code{c(mean, sd)} for the empirical
#'   genome-wide standardisation.  Both the raw and standardised
#'   routes are available.
#' @return A list with \code{estimate}, \code{xpehh_unstandardized},
#'   \code{I_A}, \code{I_B}, \code{truncated_a}, \code{truncated_b},
#'   \code{standardized}, \code{core}, \code{method}.
#' @references Sabeti, P. C. et al. (2007). Genome-wide detection and
#'   characterization of positive selection in human populations.
#'   Nature, 449, 913-918.
#' @export
morie_xpehh1 <- function(hapA, hapB, core, positions = NULL,
                         min_ehh = 0.05, standardize = NULL) {
  decA <- morie_ehhdec(hapA, core, positions)
  decB <- morie_ehhdec(hapB, core, positions)
  if (length(decA$positions) != length(decB$positions))
    stop("populations must cover the same SNPs")
  pos <- decA$positions
  me <- as.numeric(min_ehh)
  A_l <- .mor_ihh_one_side(pos, decA$ehhs, decA$core, -1, me)
  A_r <- .mor_ihh_one_side(pos, decA$ehhs, decA$core, +1, me)
  B_l <- .mor_ihh_one_side(pos, decB$ehhs, decB$core, -1, me)
  B_r <- .mor_ihh_one_side(pos, decB$ehhs, decB$core, +1, me)
  IA <- A_l$area + A_r$area
  IB <- B_l$area + B_r$area
  if (IA <= 0 || IB <= 0) stop("degenerate EHH curve: zero integrated area")
  u <- log(IA / IB)
  if (!is.null(standardize)) {
    m <- as.numeric(standardize[1]); s <- as.numeric(standardize[2])
    if (s <= 0) stop("standardize sd must be positive")
    est <- (u - m) / s
    std <- TRUE
  } else {
    est <- u
    std <- FALSE
  }
  list(estimate = est, xpehh_unstandardized = u, I_A = IA, I_B = IB,
       truncated_a = A_l$truncated || A_r$truncated,
       truncated_b = B_l$truncated || B_r$truncated,
       standardized = std, core = decA$core,
       method = "XP-EHH (Sabeti 2007): ln(I_A/I_B) of integrated site-EHH")
}
