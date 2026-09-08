# SPDX-License-Identifier: AGPL-3.0-or-later
#
# SNP-BLUP additive genomic prediction (Snpblr). Bit-identical mirror
# of src/morie/fn/snpblr.py.

#' SNP-BLUP additive genomic prediction
#'
#' Model (Meuwissen, Hayes and Goddard 2001, BLUP estimation, p. 1822):
#' \eqn{y = 1_n \mu + Z u + e} with \eqn{u \sim N(0, \sigma_u^2 I)},
#' \eqn{e \sim N(0, \sigma_e^2 I)}, solved from Henderson's mixed-model
#' equations (Henderson 1975; as printed in Montesinos-Lopez et al.
#' 2022 ch. 5.2):
#' \eqn{\[n, 1'Z; Z'1, Z'Z + \lambda I\] \[\mu; u\] = \[1'y; Z'y\]} with
#' \eqn{\lambda = \sigma_e^2/\sigma_u^2}. Z is the VanRaden-centred
#' marker matrix \eqn{Z_{ij} = M_{ij} - 2 p_j} (VanRaden 2008 Method 1
#' centring), and GEBV = Z u-hat.
#'
#' If \code{h2} is given instead of \code{lam}, the ratio is derived
#' from the VanRaden variance split
#' \eqn{\sigma_g^2 = 2 \sum_j p_j (1-p_j) \sigma_u^2}:
#' \eqn{\lambda = ((1-h^2)/h^2) \, 2 \sum_j p_j (1-p_j)}.
#'
#' @param y Phenotype vector.
#' @param M Genotype matrix coded 0/1/2 (individuals by markers).
#' @param lam Shrinkage ratio sigma_e^2 / sigma_u^2. Give exactly one
#'   of \code{lam} and \code{h2}.
#' @param h2 Heritability used to derive \code{lam}.
#' @param freq Optional allele frequencies; column means over 2 by
#'   default.
#' @return List with \code{estimate} (GEBV vector), \code{u} (marker
#'   effects), \code{mu}, \code{lam}, \code{sum2pq}, \code{freq},
#'   \code{n}, \code{m}, \code{method}.
#' @references Meuwissen, T. H. E., Hayes, B. J. and Goddard, M. E.
#'   (2001). Prediction of total genetic value using genome-wide dense
#'   marker maps. Genetics 157(4), 1819-1829, sec. BLUP estimation
#'   p. 1822 (fetched-wave3 PDF). Henderson, C. R. (1975). Biometrics
#'   31(2), 423-447. VanRaden, P. M. (2008). Journal of Dairy Science
#'   91(11), 4414-4423, via Montesinos-Lopez et al. (2022) Multivariate
#'   Statistical Machine Learning Methods for Genomic Prediction,
#'   Springer, sec. 2.4 and ch. 5.2 (local split PDFs).
#' @export
Snpblr <- function(y, M, lam = NULL, h2 = NULL, freq = NULL) {
  y <- as.numeric(y)
  Mm <- as.matrix(M)
  storage.mode(Mm) <- "double"
  n <- nrow(Mm)
  m <- ncol(Mm)
  if (length(y) != n) stop("y and M row count differ", call. = FALSE)
  if (is.null(lam) == is.null(h2)) {
    stop("give exactly one of lam or h2", call. = FALSE)
  }
  p <- if (!is.null(freq)) as.numeric(freq) else colSums(Mm) / (2 * n)
  sum2pq <- 2 * sum(p * (1 - p))
  if (is.null(lam)) {
    h2 <- as.numeric(h2)
    if (!(h2 > 0 && h2 < 1)) stop("h2 must be in (0, 1)", call. = FALSE)
    lam <- (1 - h2) / h2 * sum2pq
  }
  lam <- as.numeric(lam)
  if (lam <= 0) stop("lam must be positive", call. = FALSE)
  Z <- sweep(Mm, 2, 2 * p, "-")
  C <- matrix(0, m + 1, m + 1)
  C[1, 1] <- n
  zsum <- colSums(Z)
  C[1, -1] <- zsum
  C[-1, 1] <- zsum
  C[-1, -1] <- crossprod(Z) + lam * diag(m)
  rhs <- c(sum(y), as.numeric(crossprod(Z, y)))
  sol <- solve(C, rhs)
  mu <- sol[1]
  u <- sol[-1]
  gebv <- as.numeric(Z %*% u)
  list(
    estimate = gebv, u = u, mu = mu, lam = lam, sum2pq = sum2pq,
    freq = p, n = as.integer(n), m = as.integer(m),
    method = "SNP-BLUP (Meuwissen 2001 BLUP; Henderson MME; VanRaden centring)")
}
