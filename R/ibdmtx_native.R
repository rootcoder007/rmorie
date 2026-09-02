# Pairwise identity-by-descent by the PLINK method of moments.
# Source: Purcell, S., Neale, B., Todd-Brown, K., Thomas, L.,
# Ferreira, M. A. R., Bender, D., Maller, J., Sklar, P., de Bakker,
# P. I. W., Daly, M. J. and Sham, P. C. (2007), PLINK: a tool set for
# whole-genome association and population-based linkage analyses,
# American Journal of Human Genetics 81(3), 559-575.
#
# Their Table 1 gives P(I | Z), the probability of each IBS state I
# given each IBD state Z for one SNP, as a function of the allele
# frequencies WITH the ascertainment correction: with T_A nonmissing
# alleles of which X are A and Y are a (p = X/T_A, q = Y/T_A), the
# entries carry factors (X-1)/X, (X-2)/X, ... and
# T_A/(T_A-1), T_A/(T_A-2), T_A/(T_A-3).  Observed IBS counts are then
# equated to their expectations and solved forward for Z0, Z1, Z2
# (their "method of moments"), with the bounding rules of their
# p. 566 applied when a raw estimate leaves the simplex.
#
# Native implementation mirroring Python morie.fn.ibdmtx exactly:
# same per-SNP table construction, same skip rules (T_A < 4 or a
# monomorphic SNP), same forward substitution, same bounding order,
# and the same final projection onto the one-parameter
# (1-pi)^2, 2 pi (1-pi), pi^2 curve when pi^2 <= Z2.

#' IBS-given-IBD probabilities for one SNP (PLINK Table 1)
#'
#' Returns the 3 x 3 matrix \code{P[z + 1, s + 1] = P(IBS = s | IBD =
#' z)} for a SNP with \code{X} copies of one allele and \code{Y} of
#' the other among \code{T} nonmissing alleles, including the
#' ascertainment correction of Purcell et al. (2007), Table 1.
#'
#' @param X,Y Allele counts, \code{X + Y == T}.
#' @param T Total nonmissing alleles, at least 4.
#' @return A 3 x 3 matrix, rows indexed by IBD state 0, 1, 2.
#' @references Purcell, S. et al. (2007). PLINK. American Journal of
#'   Human Genetics, 81(3), 559-575, Table 1.
#' @export
#' @examples
#' morie_p_ibs_given_ibd(X = 2, Y = 2, T = 4)
morie_p_ibs_given_ibd <- function(X, Y, T) {
  X <- as.numeric(X); Y <- as.numeric(Y); T <- as.numeric(T)
  if (T < 4 || X + Y != T) stop("need T = X + Y >= 4")
  p <- X / T; q <- Y / T
  c3 <- (T / (T - 1)) * (T / (T - 2)) * (T / (T - 3))
  c2 <- (T / (T - 1)) * (T / (T - 2))
  p00 <- 2 * p * p * q * q * ((X - 1) / X) * ((Y - 1) / Y) * c3
  p10 <- (4 * p^3 * q * ((X - 1) / X) * ((X - 2) / X) +
          4 * p * q^3 * ((Y - 1) / Y) * ((Y - 2) / Y)) * c3
  p20 <- (p^4 * ((X - 1) / X) * ((X - 2) / X) * ((X - 3) / X) +
          q^4 * ((Y - 1) / Y) * ((Y - 2) / Y) * ((Y - 3) / Y) +
          4 * p * p * q * q * ((X - 1) / X) * ((Y - 1) / Y)) * c3
  p01 <- 0
  p11 <- (2 * p * p * q * ((X - 1) / X) +
          2 * p * q * q * ((Y - 1) / Y)) * c2
  p21 <- (p^3 * ((X - 1) / X) * ((X - 2) / X) +
          q^3 * ((Y - 1) / Y) * ((Y - 2) / Y) +
          p * p * q * ((X - 1) / X) +
          p * q * q * ((Y - 1) / Y)) * c2
  matrix(c(p00, p10, p20, p01, p11, p21, 0, 0, 1), 3, 3, byrow = TRUE)
}

#' Pairwise IBD matrix (PLINK method of moments)
#'
#' For every pair of individuals, solves the observed IBS counts
#' against their expectations under Purcell et al.'s (2007) Table 1 to
#' get the IBD sharing probabilities \eqn{Z_0, Z_1, Z_2}, and reports
#' \eqn{\hat\pi = Z_1/2 + Z_2}, the proportion of the genome shared
#' IBD.  Expected values for a full sib pair are
#' \eqn{(0.25, 0.5, 0.25)} with \eqn{\hat\pi = 0.5}; for a
#' parent-offspring pair \eqn{(0, 1, 0)}, also \eqn{\hat\pi = 0.5};
#' for unrelated individuals \eqn{(1, 0, 0)} with \eqn{\hat\pi = 0}.
#'
#' @param G Genotype matrix, individuals by SNPs, coded 0/1/2 (any
#'   other value is treated as missing).
#' @return A list with \code{estimate} (the \eqn{\hat\pi} matrix),
#'   \code{Z0}, \code{Z1}, \code{Z2}, \code{ibs_counts},
#'   \code{n_snps_used}, \code{n}, \code{m}, \code{method}.
#' @references Purcell, S. et al. (2007). PLINK. American Journal of
#'   Human Genetics, 81(3), 559-575.
#' @export
#' @examples
#' set.seed(1)
#' G <- matrix(sample(0:2, 30 * 12, replace = TRUE), 30, 12)
#' dim(morie_ibdmtx(G))
morie_ibdmtx <- function(G) {
  rows <- as.matrix(G)
  storage.mode(rows) <- "double"
  n <- nrow(rows)
  if (n < 2L) stop("need at least 2 individuals")
  m <- ncol(rows)
  valid <- c(0, 1, 2)
  tables <- vector("list", m)
  used <- 0L
  for (j in seq_len(m)) {
    obs <- rows[, j][rows[, j] %in% valid]
    T <- 2 * length(obs)
    Ya <- sum(obs)
    Xa <- T - Ya
    if (T < 4 || Xa == 0 || Ya == 0) next
    tables[[j]] <- morie_p_ibs_given_ibd(Xa, Ya, T)
    used <- used + 1L
  }
  pihat <- matrix(1, n, n)
  Z0 <- matrix(0, n, n); Z1 <- matrix(0, n, n); Z2 <- matrix(1, n, n)
  counts_out <- list()
  for (i in seq_len(n - 1L)) {
    for (k in seq.int(i + 1L, n)) {
      Nobs <- numeric(3)
      Nexp <- matrix(0, 3, 3)
      for (j in seq_len(m)) {
        if (is.null(tables[[j]])) next
        g1 <- rows[i, j]; g2 <- rows[k, j]
        if (!(g1 %in% valid) || !(g2 %in% valid)) next
        ibs <- 2 - abs(g1 - g2)
        Nobs[ibs + 1L] <- Nobs[ibs + 1L] + 1
        Nexp <- Nexp + tables[[j]]
      }
      if (Nexp[1, 1] <= 0 || Nexp[2, 2] <= 0)
        stop("no informative SNPs for a pair")
      z0 <- Nobs[1] / Nexp[1, 1]
      z1 <- (Nobs[2] - z0 * Nexp[1, 2]) / Nexp[2, 2]
      z2 <- (Nobs[3] - z0 * Nexp[1, 3] - z1 * Nexp[2, 3]) / Nexp[3, 3]
      # bounding, Purcell et al. 2007 p. 566
      if (z0 > 1) {
        z0 <- 1; z1 <- 0; z2 <- 0
      } else if (z0 < 0) {
        z0 <- 0
        s <- z1 + z2
        if (s > 0) { z1 <- z1 / s; z2 <- z2 / s }
      }
      if (z1 < 0) {
        z1 <- 0
        s <- z0 + z2
        if (s > 0) { z0 <- z0 / s; z2 <- z2 / s }
      }
      if (z2 < 0) {
        z2 <- 0
        s <- z0 + z1
        if (s > 0) { z0 <- z0 / s; z1 <- z1 / s }
      }
      pi_ <- 0.5 * z1 + z2
      if (pi_ * pi_ <= z2) {
        z0 <- (1 - pi_)^2
        z1 <- 2 * pi_ * (1 - pi_)
        z2 <- pi_ * pi_
      }
      pihat[i, k] <- pihat[k, i] <- pi_
      Z0[i, k] <- Z0[k, i] <- z0
      Z1[i, k] <- Z1[k, i] <- z1
      Z2[i, k] <- Z2[k, i] <- z2
      counts_out[[length(counts_out) + 1L]] <-
        c(i - 1L, k - 1L, Nobs[1], Nobs[2], Nobs[3])
    }
  }
  list(estimate = pihat, Z0 = Z0, Z1 = Z1, Z2 = Z2,
       ibs_counts = counts_out, n_snps_used = used, n = n, m = m,
       method = paste("Pairwise IBD (Purcell 2007 PLINK method of",
                      "moments, Table 1)"))
}
