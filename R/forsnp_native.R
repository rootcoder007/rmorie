# Forensic DNA match probability + likelihood ratio.
# Source: Buckleton, Triggs & Walsh (2005), Forensic DNA Evidence
# Interpretation, CRC Press, Ch. 3 (product rule + NRC II
# Recommendation 4.10 subpopulation correction)
# (fetched-wave3/Forensic_DNA_Evidence_Interpretation..pdf).
# Mirrors Python morie.fn.forsnp exactly.

#' .forsnp_locus
#'
#' A step of the forsnp_native implementation. Called by \code{morie_forsnp}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param a1 See Usage.
#' @param a2 See Usage.
#' @param fr A vector; indexed elementwise.
#' @param theta Numeric; combined arithmetically in the body.
#' @return A numeric value.
#' @export
.forsnp_locus <- function(a1, a2, fr, theta) {
  p1 <- as.numeric(fr[[a1]])
  if (a1 == a2) {
    num <- (2 * theta + (1 - theta) * p1) * (3 * theta + (1 - theta) * p1)
    return(num / ((1 + theta) * (1 + 2 * theta)))
  }
  p2 <- as.numeric(fr[[a2]])
  num <- 2 * (theta + (1 - theta) * p1) * (theta + (1 - theta) * p2)
  num / ((1 + theta) * (1 + 2 * theta))
}

#' Forensic DNA random-match probability and likelihood ratio
#'
#' Product rule over loci with the NRC II Recommendation 4.10
#' subpopulation correction (theta): homozygote
#' [2t+(1-t)p][3t+(1-t)p]/((1+t)(1+2t)); heterozygote
#' 2[t+(1-t)p1][t+(1-t)p2]/((1+t)(1+2t)); LR = 1/RMP.  theta = 0
#' recovers the Hardy-Weinberg product rule.
#'
#' @param genotype List of length-2 allele vectors per locus.
#' @param freqs List of per-locus named allele-frequency vectors.
#' @param theta Coancestry coefficient.
#' @return A list with elements \code{rmp}, \code{lr},
#'   \code{locus_rmp}, \code{n_loci}, \code{theta}, \code{method}.
#' @references Buckleton, J., Triggs, C. M. and Walsh, S. J. (2005).
#'   Forensic DNA Evidence Interpretation. CRC Press.
#' @export
morie_forsnp <- function(genotype, freqs, theta = 0) {
  if (length(genotype) != length(freqs) || !length(genotype)) {
    stop("genotype and freqs must be paired, non-empty")
  }
  theta <- as.numeric(theta)
  if (theta < 0 || theta >= 1) stop("theta must be in [0, 1)")
  locus <- numeric(length(genotype))
  rmp <- 1
  for (i in seq_along(genotype)) {
    a1 <- as.character(genotype[[i]][1])
    a2 <- as.character(genotype[[i]][2])
    fr <- freqs[[i]]
    if (is.null(fr[[a1]]) || is.null(fr[[a2]])) {
      stop("allele frequency missing")
    }
    locus[i] <- .forsnp_locus(a1, a2, fr, theta)
    rmp <- rmp * locus[i]
  }
  if (rmp <= 0) stop("zero match probability")
  list(rmp = rmp, lr = 1 / rmp, locus_rmp = locus,
       n_loci = length(genotype), theta = theta,
       method = "forensic RMP/LR, NRC II 4.10 (Buckleton 2005)")
}
