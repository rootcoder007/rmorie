# SPDX-License-Identifier: AGPL-3.0-or-later
#' Narrow-sense heritability from LMM variance components
#'
#' Montesinos Lopez, Montesinos Lopez and Crossa (2022), Multivariate
#' Statistical Machine Learning Methods for Genomic Prediction, Springer,
#' volume [Pages 141-170], Chapter 5, Section 5.3, equation (5.3), read as a
#' rendered page: the genomic mixed model is Y = 1_n mu + Z_L b + e with
#' b ~ N_J(0, sigma_g^2 G) and R = sigma^2 I_n, so the phenotypic variance of
#' a line is sigma_g^2 + sigma^2.
#'
#' Not in the book: all seventeen page-range volumes and the index
#' ([Pages 683-691]) were searched and the book never writes the ratio down;
#' "heritability" occurs only in prose.  The ratio
#' h^2 = sigma_g^2 / (sigma_g^2 + sigma_e^2) is from de los Campos, Sorensen
#' and Gianola (2015), Genomic heritability: what is it?, PLoS Genetics 11(5),
#' e1005048.  The decomposition is the book's, the ratio is theirs.
#'
#' @param sigma_g2 genetic variance component(s), non-negative.
#' @param sigma_e2 residual variance component(s), non-negative; recycled.
#' @return list: estimate, h2, sigma_p2, n, method.
#' @keywords internal
#' @examples
#' H2est(1, 0)$estimate
#' @export
H2est <- function(sigma_g2, sigma_e2) {
  g <- .s03vec(sigma_g2)
  e <- .s03vec(sigma_e2)
  if (length(g) == 0L || length(e) == 0L) {
    stop("heritability_lmm: both variance components are required")
  }
  if (length(g) != length(e) && length(g) != 1L && length(e) != 1L) {
    stop("heritability_lmm: sigma_g2 and sigma_e2 have incompatible lengths")
  }
  n <- max(length(g), length(e))
  h2 <- numeric(n)
  sp <- numeric(n)
  for (i in seq_len(n)) {
    a <- g[((i - 1L) %% length(g)) + 1L]
    b <- e[((i - 1L) %% length(e)) + 1L]
    if (a < 0 || b < 0) stop("heritability_lmm: variance components must be non-negative")
    p <- a + b
    if (p <= 0) stop("heritability_lmm: phenotypic variance is zero")
    sp[i] <- p
    h2[i] <- a / p
  }
  list(estimate = h2[1], h2 = h2, sigma_p2 = sp, n = n,
       method = "h2 = sigma_g^2/(sigma_g^2+sigma_e^2); Ch 5 eq. (5.3) decomposition, ratio from de los Campos et al. (2015)")
}
