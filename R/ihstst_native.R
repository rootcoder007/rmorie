# Integrated haplotype score (iHS).
# Source: Voight, B. F., Kudaravalli, S., Wen, X. and Pritchard, J. K.
# (2006), A map of recent positive selection in the human genome,
# PLoS Biology 4(3), e72.  Their unstandardised statistic is
# ln(iHH_A / iHH_D), the log ratio of the integrated EHH curves of the
# ancestral and derived core alleles, each integrated by the
# trapezoid rule outward from the core until EHH falls below a
# threshold (0.05 in the paper).  Standardisation is done empirically
# within derived-allele-frequency bins, which is why the mean and sd
# are supplied by the caller rather than computed here.
#
# Native implementation mirroring Python morie.fn.ihstst exactly: the
# 0/1 core coding is treated as ancestral/derived in that order, so
# iHH_A comes from the ehh0 curve and iHH_D from ehh1.

#' Integrated haplotype score (iHS)
#'
#' Computes \eqn{\ln(iHH_A / iHH_D)} at a core SNP (Voight et al.
#' 2006).  A long haplotype carrying the derived allele drives
#' \eqn{iHH_D} up and the score negative; the reverse gives a positive
#' score.  Both routes the paper describes are available: the raw
#' statistic, and the frequency-binned standardised score when
#' \code{standardize} supplies the bin mean and standard deviation.
#'
#' @param hap Haplotype matrix, chromosomes by sites, coded 0/1 with 0
#'   ancestral at the core.
#' @param core Core site index, 0-based.
#' @param positions Optional physical positions.
#' @param min_ehh EHH threshold at which integration stops, default
#'   0.05 as in the paper.
#' @param standardize Optional \code{c(mean, sd)} of the empirical
#'   distribution in this SNP's derived-allele-frequency bin.
#' @return A list with \code{estimate}, \code{ihs_unstandardized},
#'   \code{ihh_a}, \code{ihh_d}, \code{daf}, \code{truncated_a},
#'   \code{truncated_d}, \code{standardized}, \code{core},
#'   \code{method}.
#' @references Voight, B. F., Kudaravalli, S., Wen, X. and Pritchard,
#'   J. K. (2006). A map of recent positive selection in the human
#'   genome. PLoS Biology, 4(3), e72.
#' @export
morie_ihstst <- function(hap, core, positions = NULL, min_ehh = 0.05,
                         standardize = NULL) {
  dec <- morie_ehhdec(hap, core, positions)
  pos <- dec$positions
  if (dec$n0 < 2L || dec$n1 < 2L)
    stop("both core alleles need at least 2 chromosomes")
  me <- as.numeric(min_ehh)
  A_l <- .mor_ihh_one_side(pos, dec$ehh0, dec$core, -1, me)
  A_r <- .mor_ihh_one_side(pos, dec$ehh0, dec$core, +1, me)
  D_l <- .mor_ihh_one_side(pos, dec$ehh1, dec$core, -1, me)
  D_r <- .mor_ihh_one_side(pos, dec$ehh1, dec$core, +1, me)
  ihh_a <- A_l$area + A_r$area
  ihh_d <- D_l$area + D_r$area
  if (ihh_a <= 0 || ihh_d <= 0)
    stop("degenerate EHH curve: zero integrated area")
  u <- log(ihh_a / ihh_d)
  if (!is.null(standardize)) {
    m <- as.numeric(standardize[1])
    s <- as.numeric(standardize[2])
    if (s <= 0) stop("standardize sd must be positive")
    est <- (u - m) / s
    std <- TRUE
  } else {
    est <- u
    std <- FALSE
  }
  list(estimate = est, ihs_unstandardized = u, ihh_a = ihh_a, ihh_d = ihh_d,
       daf = dec$n1 / dec$n,
       truncated_a = A_l$truncated || A_r$truncated,
       truncated_d = D_l$truncated || D_r$truncated,
       standardized = std, core = dec$core,
       method = paste("iHS (Voight 2006 eq. 1): ln(iHH_A/iHH_D),",
                      "trapezoid EHH to < min_ehh"))
}
