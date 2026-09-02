# SPDX-License-Identifier: AGPL-3.0-or-later
#
# GO term enrichment by the hypergeometric upper tail (Goenr).
# Bit-identical mirror of src/morie/fn/goenr.py. Anchored against
# stats::phyper upper tail and a hand-computed 2x2 example.

#' GO term enrichment (hypergeometric upper tail)
#'
#' For each GO node, with N genes in the background, M annotated to
#' the node, a study list of n genes of which k are annotated, the
#' p-value is the upper-tail hypergeometric sum
#' \eqn{P = 1 - \sum_{i=0}^{k-1} C(M,i) C(N-M,n-i) / C(N,n)}, the
#' probability of k or more annotated genes by chance (one-tailed
#' Fisher exact test). Optional Bonferroni correction multiplies each
#' p-value by the number of terms and caps at 1.
#'
#' @param hits Annotated genes in the study list, one entry per term.
#' @param list_size Size n of the study list.
#' @param term_size Genes M annotated to each term in the background.
#' @param background_size Total genes N in the background.
#' @param correction Either \code{"none"} or \code{"bonferroni"}.
#' @return List with \code{pvalue}, \code{padj}, \code{expected},
#'   \code{fold_enrichment}, \code{hits}, \code{term_size}, \code{n},
#'   \code{N}, \code{method}.
#' @references Boyle, E. I., Weng, S., Gollub, J., Jin, H., Botstein,
#'   D., Cherry, J. M. and Sherlock, G. (2004),
#'   GO::TermFinder--open source software for accessing Gene Ontology
#'   information and finding significantly enriched Gene Ontology
#'   terms associated with a list of genes, Bioinformatics 20(18),
#'   3710-3715. Hypergeometric formula and Bonferroni correction,
#'   Algorithm section, p. 3711. Source: PMC3037731 (saved as
#'   library/pdf/fetched-wave3/Boyle-2004-GO-TermFinder-Bioinformatics.html).
#' @export
#' @examples
#' set.seed(1)
#' r <- Goenr(hits = rnorm(10), list_size = 8L, term_size = 8L, background_size = 8L); TRUE
Goenr <- function(hits, list_size, term_size, background_size,
                  correction = c("none", "bonferroni")) {
  correction <- match.arg(correction)
  k <- as.numeric(hits)
  M <- as.numeric(term_size)
  if (length(M) == 1L && length(k) > 1L) M <- rep(M, length(k))
  if (length(M) != length(k)) {
    stop("hits and term_size must have equal length", call. = FALSE)
  }
  n <- as.integer(list_size)
  N <- as.integer(background_size)
  if (n > N) stop("list_size cannot exceed background_size", call. = FALSE)
  lchoose_ <- function(nn, kk) {
    ifelse(kk < 0 | kk > nn, -Inf,
           lgamma(nn + 1) - lgamma(kk + 1) - lgamma(nn - kk + 1))
  }
  upper_tail <- function(kj, Mj) {
    lo <- max(0L, n - (N - Mj))
    hi <- min(n, Mj)
    if (kj <= lo) return(1)
    if (kj > hi) return(0)
    denom <- lchoose_(N, n)
    up <- sum(exp(lchoose_(Mj, kj:hi) + lchoose_(N - Mj, n - (kj:hi)) - denom))
    lw <- sum(exp(lchoose_(Mj, lo:(kj - 1)) +
                    lchoose_(N - Mj, n - (lo:(kj - 1))) - denom))
    if (up <= lw) min(1, up) else min(1, max(0, 1 - lw))
  }
  nt <- length(k)
  pvalue <- numeric(nt)
  for (j in seq_len(nt)) {
    kj <- as.integer(k[j]); Mj <- as.integer(M[j])
    if (kj > min(n, Mj)) {
      stop("hits cannot exceed min(list_size, term_size)", call. = FALSE)
    }
    pvalue[j] <- upper_tail(kj, Mj)
  }
  padj <- if (correction == "bonferroni") {
    pmin(1, pmax(0, pvalue * nt))
  } else {
    pvalue
  }
  expected <- n * M / N
  fold <- ifelse(M > 0, (k / n) / (M / N), NaN)
  list(pvalue = pvalue, padj = padj, expected = expected,
       fold_enrichment = fold, hits = k, term_size = M, n = n, N = N,
       method = "GO enrichment, hypergeometric upper tail (Boyle et al. 2004)")
}
