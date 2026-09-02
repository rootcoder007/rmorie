# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Linkage disequilibrium r^2 (Ldcmpr). Bit-identical mirror of
# src/morie/fn/ldcmpr.py. The unphased path reuses Dprime() (twoldp.R)
# for the fixed-iteration EM phase resolution.

#' Linkage disequilibrium r-squared between two biallelic loci
#'
#' With haplotype frequency pAB and allele frequencies pA, pB
#' (pa = 1 - pA, pb = 1 - pB), \eqn{D = p_{AB} - p_A p_B} and
#' \eqn{r^2 = D^2 / (p_A p_a p_B p_b)}, the squared correlation of
#' allelic state over haplotypes (Hill and Robertson 1968).
#'
#' Two input conventions: \code{phased = TRUE} takes 0/1 allele
#' indicators on aligned haplotypes and computes r-squared exactly;
#' \code{phased = FALSE} (default) takes unphased 0/1/2 genotypes and
#' estimates pAB by the fixed-iteration EM of \code{Dprime}, oriented
#' to the major allele at each locus (r-squared is invariant to the
#' flip). The squared Pearson correlation of the genotype counts is
#' also reported as \code{r2_genotypic}; this is what PLINK 1.9
#' computes for its r-squared reports (genotype allele counts, phase
#' not considered).
#'
#' @param geno1 First locus: allele indicators (phased) or genotype
#'   counts (unphased).
#' @param geno2 Second locus, same convention.
#' @param phased Logical input convention flag.
#' @return List with \code{estimate} (r-squared), \code{r}, \code{D},
#'   \code{Dprime}, \code{pA}, \code{pB}, \code{pAB},
#'   \code{r2_genotypic} (unphased only), \code{n}, \code{method}.
#' @references Hill, W. G. and Robertson, A. (1968). Linkage
#'   disequilibrium in finite populations. Theoretical and Applied
#'   Genetics 38(6), 226-231 (r-squared definition, sec. 2).
#'   Lewontin, R. C. (1964). Genetics 49(1), 49-67 (D, D-prime).
#'   PLINK 1.9 LD documentation (genotype-allele-count correlation),
#'   fetched 2026-08-09. EM phase resolution follows CRAN package
#'   genetics R/LD.R as documented in twoldp.R.
#' @export
#' @examples
#' V <- c(1, 2, 3, 4, 5, 6, 7, 8)
#' Ldcmpr(V, V)
Ldcmpr <- function(geno1, geno2, phased = FALSE) {
  if (isTRUE(phased)) {
    a <- as.numeric(geno1); b <- as.numeric(geno2)
    if (length(a) != length(b)) stop("geno1 and geno2 must be the same length", call. = FALSE)
    n <- length(a)
    if (n < 2L) stop("need at least 2 haplotypes", call. = FALSE)
    if (!all(a %in% c(0, 1)) || !all(b %in% c(0, 1))) {
      stop("phased inputs must be coded 0/1", call. = FALSE)
    }
    pA <- sum(a) / n; pB <- sum(b) / n
    pAB <- sum(a * b) / n
    d <- pAB - pA * pB
    pa <- 1 - pA; pb <- 1 - pB
    denom <- pA * pa * pB * pb
    r <- if (denom > 0) d / sqrt(denom) else NaN
    dmax <- min(pA * pb, pa * pB)
    dmin <- max(-pA * pB, -pa * pb)
    dp <- if (d > 0) { if (dmax > 0) d / dmax else NaN
    } else if (d < 0) { if (dmin < 0) d / dmin else NaN } else 0
    return(list(
      estimate = if (is.nan(r)) NaN else r * r,
      r = r, D = d, Dprime = dp, pA = pA, pB = pB, pAB = pAB,
      n = as.integer(n),
      method = "LD r^2 (Hill-Robertson 1968), phased haplotypes"))
  }
  base <- Dprime(geno1, geno2)
  g1 <- as.numeric(geno1); g2 <- as.numeric(geno2)
  ok <- g1 %in% c(0, 1, 2) & g2 %in% c(0, 1, 2)
  x <- g1[ok]; y <- g2[ok]
  n <- length(x)
  mx <- mean(x); my <- mean(y)
  sxy <- sum((x - mx) * (y - my))
  sxx <- sum((x - mx)^2); syy <- sum((y - my)^2)
  r2g <- if (sxx > 0 && syy > 0) (sxy * sxy) / (sxx * syy) else NaN
  list(
    estimate = base$r2, r = base$r, D = base$D, Dprime = base$estimate,
    pA = base$pA, pB = base$pB, pAB = base$pAB,
    r2_genotypic = r2g, n = base$n,
    method = "LD r^2 (Hill-Robertson 1968), EM-phased genotypes")
}
