# SPDX-License-Identifier: AGPL-3.0-or-later
.t4_EM_ITERS <- 500L

#' Lewontin's D' for two biallelic loci from unphased genotypes.
#'
#' Formula: with \eqn{p_A, p_B} the major-allele frequencies,
#' \eqn{p_a = 1-p_A}, \eqn{p_b = 1-p_B} and \eqn{p_{AB}} the haplotype
#' frequency, \eqn{D = p_{AB} - p_A p_B},
#' \eqn{D_{max} = \min(p_A p_b,\, p_a p_B)},
#' \eqn{D_{min} = \max(-p_A p_B,\, -p_a p_b)}, and
#' \eqn{D' = D/D_{max}} if \eqn{D > 0} else \eqn{D/D_{min}}.
#' Normalising by the attainable extreme is the whole content of the
#' proposal: raw D is bounded by the allele frequencies and cannot be
#' compared across loci, whereas D' reaches 1 exactly when one
#' haplotype is absent.  Note the sign convention, which is the one
#' genetics::LD uses and is easy to misread: \eqn{D_{min}} is negative,
#' so dividing a negative D by it gives a positive D'.  \code{estimate}
#' is therefore the normalised magnitude in \[0, 1\] and the direction is
#' carried by the sign of \code{D}; sources that put D' on \[-1, 1\] mean
#' \code{sign(D) * estimate}.  Genotypes are
#' unphased, so only the double heterozygote is ambiguous; it is
#' resolved by EM run for a fixed 500 iterations with no convergence
#' test, so the answer is deterministic and identical in both arms.
#'
#' @param geno1,geno2 Genotypes coded as the allele count 0, 1, 2.
#'   Individuals miscoded or missing at either locus are dropped
#'   pairwise.
#' @return List with \code{estimate} (D'), \code{D}, \code{pAB},
#'   \code{pA}, \code{pB}, \code{Dmax}, \code{Dmin}, \code{r},
#'   \code{r2}, \code{n}, \code{method}.
#' @references Lewontin (1964), Genetics 49:49-67.  The Genetics PDF at PMC could not be retrieved from this host (the fetch returned a 1.8 kB error page), so the coded form was read from Warnes and Leisch's CRAN package genetics, R/LD.R, which gives Dmin, Dmax and estDp verbatim.  genetics maximises the same likelihood with optimize(); a fixed-iteration EM is used here because a golden-section search is not reproducible across language arms.
#' @export
#' @examples
#' V <- c(1, 2, 3, 4, 5, 6, 7, 8)
#' Dprime(V, V)
Dprime <- function(geno1, geno2) {
  g1 <- .t4_vec(geno1); g2 <- .t4_vec(geno2)
  if (length(g1) != length(g2)) stop("geno1 and geno2 must be the same length")
  keep <- g1 == round(g1) & g2 == round(g2) & g1 >= 0 & g1 <= 2 & g2 >= 0 & g2 <= 2
  a <- as.integer(g1[keep]); b <- as.integer(g2[keep]); n <- length(a)
  if (n < 2L) stop("need at least 2 complete genotype pairs")
  pA <- sum(a) / (2 * n); pB <- sum(b) / (2 * n)
  if (pA < 0.5) { a <- 2L - a; pA <- 1 - pA }
  if (pB < 0.5) { b <- 2L - b; pB <- 1 - pB }
  pa <- 1 - pA; pb <- 1 - pB
  tab <- matrix(0, 3, 3)
  for (i in seq_len(n)) tab[a[i] + 1L, b[i] + 1L] <- tab[a[i] + 1L, b[i] + 1L] + 1
  nAB <- 2 * tab[3, 3] + tab[3, 2] + tab[2, 3]
  namb <- tab[2, 2]
  dmin <- max(-pA * pB, -pa * pb)
  dmax <- min(pA * pb, pa * pB)
  pab <- pA * pB
  lo <- max(0, pA + pB - 1); hi <- min(pA, pB)
  for (it in seq_len(.t4_EM_ITERS)) {
    num <- pab * (1 - pA - pB + pab)
    den <- num + (pA - pab) * (pB - pab)
    w <- if (den > 0) num / den else 0.5
    pab <- (nAB + 2 * namb * w) / (2 * n)
    if (pab < lo) pab <- lo
    if (pab > hi) pab <- hi
  }
  d <- pab - pA * pB
  dp <- if (d > 0) { if (dmax > 0) d / dmax else NaN
       } else if (d < 0) { if (dmin < 0) d / dmin else NaN } else 0
  denom <- pA * pB * pa * pb
  r <- if (denom > 0) d / sqrt(denom) else NaN
  .t4_result(estimate = dp, D = d, pAB = pab, pA = pA, pB = pB,
             Dmax = dmax, Dmin = dmin, r = r, r2 = if (is.nan(r)) NaN else r * r,
             n = as.integer(n), method = "Lewontin D' two-locus disequilibrium")
}
