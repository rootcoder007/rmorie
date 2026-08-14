# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Selection-scan shelf, wave3 w5_04 batch 2: iHS (Ihstst), XP-EHH
# (Xpehh1), PLINK method-of-moments IBD matrix (Ibdmtx). Bit-identical
# mirrors of src/morie/fn/{ihstst,xpehh1,ibdmtx}.py, built on Ehhdec
# (ehhdec_native.R). R cores are 1-based; the Python arms are 0-based
# (parity harnesses pass core+1 on the R side).

.morie_ihh_side <- function(pos, ehh, core, side, min_ehh) {
  # Voight et al. 2006, Materials and Methods "Calculation of iHS":
  # trapezoid area outward from the core, INCLUDING the segment that
  # ends at the first marker below min_ehh.
  L <- length(pos)
  idx <- if (side > 0) {
    if (core + 1L > L) integer(0) else (core + 1L):L
  } else {
    if (core - 1L < 1L) integer(0) else (core - 1L):1L
  }
  area <- 0
  prev_p <- pos[core]; prev_e <- ehh[core]
  truncated <- TRUE
  for (j in idx) {
    area <- area + abs(pos[j] - prev_p) * 0.5 * (ehh[j] + prev_e)
    prev_p <- pos[j]; prev_e <- ehh[j]
    if (ehh[j] < min_ehh) { truncated <- FALSE; break }
  }
  list(area = area, truncated = truncated)
}

#' Integrated haplotype score (iHS) for one core SNP
#'
#' EHH decay curves for the ancestral (0) and derived (1) core-allele
#' carriers (\code{\link{Ehhdec}}), each integrated by the trapezoid
#' rule outward from the core until EHH < \code{min_ehh} (0.05 in the
#' paper), giving iHH_A and iHH_D; unstandardized iHS =
#' ln(iHH_A / iHH_D) (Voight et al. 2006, eq. 1). Pass
#' \code{standardize = c(mean, sd)} of a genome-wide reference for the
#' frequency-bin z-score of their eq. 2.
#'
#' @param hap Phased 0/1 haplotype matrix (chromosomes x SNPs);
#'   1 = derived.
#' @param core Core SNP index (1-based).
#' @param positions Optional positions; index scale by default.
#' @param min_ehh Integration threshold (default 0.05).
#' @param standardize Optional c(mean, sd) for the z-score.
#' @return List with \code{estimate}, \code{ihs_unstandardized},
#'   \code{ihh_a}, \code{ihh_d}, \code{daf}, \code{truncated_a},
#'   \code{truncated_d}, \code{standardized}, \code{core},
#'   \code{method}.
#' @references Voight, B. F., Kudaravalli, S., Wen, X. and Pritchard,
#'   J. K. (2006), PLoS Biology 4(3), e72, eqs. (1)-(2) and Materials
#'   and Methods "Calculation of iHS". Sabeti, P. C. et al. (2002),
#'   Nature 419, 832-837. Local: fetched-wave3 Voight-2006 PDF.
#' @export
Ihstst <- function(hap, core, positions = NULL, min_ehh = 0.05,
                   standardize = NULL) {
  dec <- morie_ehhdec(hap, core, positions)
  pos <- dec$positions
  if (dec$n0 < 2L || dec$n1 < 2L) {
    stop("both core alleles need at least 2 chromosomes", call. = FALSE)
  }
  me <- as.numeric(min_ehh)[1]
  aL <- .morie_ihh_side(pos, dec$ehh0, dec$core, -1L, me)
  aR <- .morie_ihh_side(pos, dec$ehh0, dec$core, +1L, me)
  dL <- .morie_ihh_side(pos, dec$ehh1, dec$core, -1L, me)
  dR <- .morie_ihh_side(pos, dec$ehh1, dec$core, +1L, me)
  ihh_a <- aL$area + aR$area
  ihh_d <- dL$area + dR$area
  if (ihh_a <= 0 || ihh_d <= 0) {
    stop("degenerate EHH curve: zero integrated area", call. = FALSE)
  }
  u <- log(ihh_a / ihh_d)
  if (!is.null(standardize)) {
    mu <- as.numeric(standardize[1]); sd_ <- as.numeric(standardize[2])
    if (sd_ <= 0) stop("standardize sd must be positive", call. = FALSE)
    est <- (u - mu) / sd_; std <- TRUE
  } else {
    est <- u; std <- FALSE
  }
  list(estimate = est, ihs_unstandardized = u, ihh_a = ihh_a,
       ihh_d = ihh_d, daf = dec$n1 / dec$n,
       truncated_a = aL$truncated || aR$truncated,
       truncated_d = dL$truncated || dR$truncated,
       standardized = std, core = dec$core,
       method = "iHS (Voight 2006 eq. 1): ln(iHH_A/iHH_D), trapezoid EHH to < min_ehh")
}

#' Cross-population extended haplotype homozygosity (XP-EHH)
#'
#' Site-wise EHH curves over all chromosomes of each population
#' (\code{\link{Ehhdec}} \code{ehhs}) integrated outward from the core
#' by the same trapezoid rule as iHS; XP-EHH = ln(I_A / I_B)
#' (Sabeti et al. 2007, Methods). Local recombination cancels in the
#' ratio. Pass \code{standardize = c(mean, sd)} to normalize.
#'
#' @param hapA,hapB Phased 0/1 haplotype matrices over the SAME SNPs.
#' @param core Core SNP index (1-based).
#' @param positions Optional positions.
#' @param min_ehh Integration threshold.
#' @param standardize Optional c(mean, sd).
#' @return List with \code{estimate}, \code{xpehh_unstandardized},
#'   \code{I_A}, \code{I_B}, \code{truncated_a}, \code{truncated_b},
#'   \code{standardized}, \code{core}, \code{method}.
#' @references Sabeti, P. C., Varilly, P. et al. (2007), Nature
#'   449(7164), 913-918, Methods pp. 5-6. Local: fetched-wave3
#'   Sabeti-2007 PDF.
#' @export
Xpehh1 <- function(hapA, hapB, core, positions = NULL, min_ehh = 0.05,
                   standardize = NULL) {
  decA <- morie_ehhdec(hapA, core, positions)
  decB <- morie_ehhdec(hapB, core, positions)
  if (length(decA$positions) != length(decB$positions)) {
    stop("populations must cover the same SNPs", call. = FALSE)
  }
  pos <- decA$positions
  me <- as.numeric(min_ehh)[1]
  aL <- .morie_ihh_side(pos, decA$ehhs, decA$core, -1L, me)
  aR <- .morie_ihh_side(pos, decA$ehhs, decA$core, +1L, me)
  bL <- .morie_ihh_side(pos, decB$ehhs, decB$core, -1L, me)
  bR <- .morie_ihh_side(pos, decB$ehhs, decB$core, +1L, me)
  IA <- aL$area + aR$area
  IB <- bL$area + bR$area
  if (IA <= 0 || IB <= 0) {
    stop("degenerate EHH curve: zero integrated area", call. = FALSE)
  }
  u <- log(IA / IB)
  if (!is.null(standardize)) {
    mu <- as.numeric(standardize[1]); sd_ <- as.numeric(standardize[2])
    if (sd_ <= 0) stop("standardize sd must be positive", call. = FALSE)
    est <- (u - mu) / sd_; std <- TRUE
  } else {
    est <- u; std <- FALSE
  }
  list(estimate = est, xpehh_unstandardized = u, I_A = IA, I_B = IB,
       truncated_a = aL$truncated || aR$truncated,
       truncated_b = bL$truncated || bR$truncated,
       standardized = std, core = decA$core,
       method = "XP-EHH (Sabeti 2007): ln(I_A/I_B) of integrated site-EHH")
}

.morie_p_ibs_ibd <- function(X, Y, T_) {
  # PLINK Table 1 with finite-sample corrections (Purcell 2007 p 566)
  X <- as.numeric(X); Y <- as.numeric(Y); T_ <- as.numeric(T_)
  if (T_ < 4 || X + Y != T_) stop("need T = X + Y >= 4", call. = FALSE)
  p <- X / T_; q <- Y / T_
  c3 <- (T_ / (T_ - 1)) * (T_ / (T_ - 2)) * (T_ / (T_ - 3))
  c2 <- (T_ / (T_ - 1)) * (T_ / (T_ - 2))
  p00 <- 2 * p * p * q * q * ((X - 1) / X) * ((Y - 1) / Y) * c3
  p10 <- (4 * p^3 * q * ((X - 1) / X) * ((X - 2) / X)
          + 4 * p * q^3 * ((Y - 1) / Y) * ((Y - 2) / Y)) * c3
  p20 <- (p^4 * ((X - 1) / X) * ((X - 2) / X) * ((X - 3) / X)
          + q^4 * ((Y - 1) / Y) * ((Y - 2) / Y) * ((Y - 3) / Y)
          + 4 * p * p * q * q * ((X - 1) / X) * ((Y - 1) / Y)) * c3
  p01 <- 0
  p11 <- (2 * p * p * q * ((X - 1) / X)
          + 2 * p * q * q * ((Y - 1) / Y)) * c2
  p21 <- (p^3 * ((X - 1) / X) * ((X - 2) / X)
          + q^3 * ((Y - 1) / Y) * ((Y - 2) / Y)
          + p * p * q * ((X - 1) / X)
          + p * q * q * ((Y - 1) / Y)) * c2
  rbind(c(p00, p10, p20), c(p01, p11, p21), c(0, 0, 1))
}

#' Pairwise identity-by-descent matrix (PLINK method of moments)
#'
#' Observed IBS-state counts per pair matched to expectations from the
#' Table 1 P(IBS | IBD) with finite-sample corrections; P(Z=0..2)
#' solved sequentially, bounded per p. 566, pi-hat = P(Z=1)/2 + P(Z=2)
#' with the biological constraint transform when pi^2 <= P(Z=2). The
#' stub's Browning-Browning (2010) citation was a misattribution
#' (fastIBD is an HMM segment method) -- recorded; this is Purcell
#' et al. (2007).
#'
#' @param G Genotype matrix coded 0/1/2 (individuals x SNPs); other
#'   values treated as missing.
#' @return List with \code{estimate} (pi-hat matrix), \code{Z0},
#'   \code{Z1}, \code{Z2}, \code{ibs_counts}, \code{n_snps_used},
#'   \code{n}, \code{m}, \code{method}.
#' @references Purcell, S., Neale, B., Todd-Brown, K. et al. (2007),
#'   American Journal of Human Genetics 81(3), 559-575, Table 1 and
#'   pp. 565-566. Local: fetched-wave3
#'   Purcell-2007-PLINK-AJHG81-559.pdf.
#' @export
Ibdmtx <- function(G) {
  Gm <- as.matrix(G); storage.mode(Gm) <- "double"
  n <- nrow(Gm); m <- ncol(Gm)
  if (n < 2L) stop("need at least 2 individuals", call. = FALSE)
  valid <- function(v) v %in% c(0, 1, 2)
  tables <- vector("list", m)
  used <- 0L
  for (j in seq_len(m)) {
    obs <- Gm[valid(Gm[, j]), j]
    T_ <- 2 * length(obs)
    Xa <- T_ - sum(obs); Ya <- sum(obs)
    if (T_ < 4 || Xa == 0 || Ya == 0) next
    tables[[j]] <- .morie_p_ibs_ibd(Xa, Ya, T_)
    used <- used + 1L
  }
  pihat <- matrix(1, n, n); Z0 <- matrix(0, n, n)
  Z1 <- matrix(0, n, n); Z2 <- matrix(1, n, n)
  counts_out <- list()
  for (i in seq_len(n - 1L)) {
    for (k in (i + 1L):n) {
      Nobs <- c(0, 0, 0)
      Nexp <- matrix(0, 3, 3)
      for (j in seq_len(m)) {
        if (is.null(tables[[j]])) next
        g1 <- Gm[i, j]; g2 <- Gm[k, j]
        if (!valid(g1) || !valid(g2)) next
        ibs <- 2L - as.integer(abs(g1 - g2))
        Nobs[ibs + 1L] <- Nobs[ibs + 1L] + 1
        Nexp <- Nexp + tables[[j]]
      }
      if (Nexp[1, 1] <= 0 || Nexp[2, 2] <= 0) {
        stop("no informative SNPs for a pair", call. = FALSE)
      }
      z0 <- Nobs[1] / Nexp[1, 1]
      z1 <- (Nobs[2] - z0 * Nexp[1, 2]) / Nexp[2, 2]
      z2 <- (Nobs[3] - z0 * Nexp[1, 3] - z1 * Nexp[2, 3]) / Nexp[3, 3]
      if (z0 > 1) { z0 <- 1; z1 <- 0; z2 <- 0 }
      else if (z0 < 0) {
        z0 <- 0; s <- z1 + z2
        if (s > 0) { z1 <- z1 / s; z2 <- z2 / s }
      }
      if (z1 < 0) { z1 <- 0; s <- z0 + z2; if (s > 0) { z0 <- z0 / s; z2 <- z2 / s } }
      if (z2 < 0) { z2 <- 0; s <- z0 + z1; if (s > 0) { z0 <- z0 / s; z1 <- z1 / s } }
      pi_ <- 0.5 * z1 + z2
      if (pi_ * pi_ <= z2) {
        z0 <- (1 - pi_)^2; z1 <- 2 * pi_ * (1 - pi_); z2 <- pi_ * pi_
      }
      pihat[i, k] <- pihat[k, i] <- pi_
      Z0[i, k] <- Z0[k, i] <- z0
      Z1[i, k] <- Z1[k, i] <- z1
      Z2[i, k] <- Z2[k, i] <- z2
      counts_out[[length(counts_out) + 1L]] <- c(i, k, Nobs)
    }
  }
  list(estimate = pihat, Z0 = Z0, Z1 = Z1, Z2 = Z2,
       ibs_counts = counts_out, n_snps_used = used, n = n, m = m,
       method = "Pairwise IBD (Purcell 2007 PLINK method of moments, Table 1)")
}
