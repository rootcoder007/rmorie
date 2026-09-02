# SPDX-License-Identifier: AGPL-3.0-or-later
#
# GSEA enrichment score (Gnsetenr). Bit-identical mirror of
# src/morie/fn/gnsetenr.py. Anchored on a hand-computed running sum
# and the p = 0 reduction to the Kolmogorov-Smirnov statistic.

#' Gene-set enrichment analysis enrichment score (GSEA)
#'
#' Genes are ranked by decreasing correlation with the phenotype.
#' Walking down the ranked list, a running sum increases by
#' \eqn{|r_j|^p / N_R} (with \eqn{N_R = \sum_{g_j \in S} |r_j|^p})
#' when gene j belongs to the set S and decreases by
#' \eqn{1/(N - N_H)} otherwise. The enrichment score ES is the
#' maximum deviation from zero of the running sum, a weighted
#' Kolmogorov-Smirnov-like statistic; p = 0 reduces to the standard
#' Kolmogorov-Smirnov statistic and p = 1 is the paper default. With
#' \code{nperm > 0} a nominal p-value is estimated by permuting the
#' gene labels of the set (pre-ranked variant), assessing positive
#' and negative scores separately against the same-sign side of the
#' permutation null as prescribed in the paper.
#'
#' @param correlations Correlation of each gene with the phenotype.
#' @param in_set Logical/0-1 membership of each gene in the set.
#' @param p Weighting exponent (default 1).
#' @param nperm Number of gene-label permutations (0 = none).
#' @param seed Seed for the permutation stream.
#' @return List with \code{es}, \code{arg_es} (0-based rank of the
#'   extreme deviation), \code{running}, \code{n}, \code{n_hits},
#'   \code{nperm}, \code{pvalue} (when \code{nperm > 0}),
#'   \code{method}.
#' @references Subramanian, A., Tamayo, P., Mootha, V. K.,
#'   Mukherjee, S., Ebert, B. L., Gillette, M. A., Paulovich, A.,
#'   Pomeroy, S. L., Golub, T. R., Lander, E. S. and Mesirov, J. P.
#'   (2005), Gene set enrichment analysis: A knowledge-based approach
#'   for interpreting genome-wide expression profiles, PNAS 102(43),
#'   15545-15550. Appendix, Enrichment Score ES(S), p. 15550;
#'   sign-separated significance, p. 15546. Local source:
#'   library/pdf/fetched-wave3/Subramanian-2005-GSEA-PNAS.pdf.
#' @export
#' @examples
#' set.seed(1)
#' Gnsetenr(correlations = rnorm(20), in_set = c(rep(TRUE, 5), rep(FALSE, 15)))
Gnsetenr <- function(correlations, in_set, p = 1, nperm = 0L,
                     seed = NULL) {
  r <- as.numeric(correlations)
  mem <- as.numeric(in_set)
  n <- length(r)
  if (length(mem) != n) {
    stop("correlations and in_set must have equal length", call. = FALSE)
  }
  ord <- order(-r, seq_len(n))
  r_s <- r[ord]
  m_s <- mem[ord] != 0
  nh <- sum(m_s)
  es_of <- function(member) {
    nhh <- sum(member)
    if (nhh == 0L || nhh == n) {
      stop("gene set must be a proper nonempty subset", call. = FALSE)
    }
    nr <- sum(abs(r_s[member])^p)
    miss_w <- 1 / (n - nhh)
    run <- 0; best <- 0; best_i <- 0L
    running <- numeric(n)
    for (i in seq_len(n)) {
      if (member[i]) {
        run <- run + if (nr > 0) abs(r_s[i])^p / nr else 1 / nhh
      } else {
        run <- run - miss_w
      }
      running[i] <- run
      if (abs(run) > abs(best)) { best <- run; best_i <- i - 1L }
    }
    list(es = best, arg = best_i, running = running)
  }
  fit <- es_of(m_s)
  out <- list(es = fit$es, arg_es = fit$arg, running = fit$running,
              n = n, n_hits = nh, nperm = as.integer(nperm),
              method = "GSEA enrichment score (Subramanian et al. 2005)")
  if (nperm > 0) {
    if (!is.null(seed)) set.seed(seed)
    same_sign <- 0L; as_extreme <- 0L
    for (b in seq_len(nperm)) {
      idx <- sample.int(n, nh)
      pm <- rep(FALSE, n)
      pm[idx] <- TRUE
      ep <- es_of(pm)$es
      if ((fit$es >= 0 && ep >= 0) || (fit$es < 0 && ep < 0)) {
        same_sign <- same_sign + 1L
        if (abs(ep) >= abs(fit$es)) as_extreme <- as_extreme + 1L
      }
    }
    out$pvalue <- if (same_sign > 0) as_extreme / same_sign else NaN
  }
  out
}
