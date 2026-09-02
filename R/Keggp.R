# SPDX-License-Identifier: AGPL-3.0-or-later
#' KEGG pathway over-representation analysis
#'
#' The standard over-representation (Fisher / hypergeometric) test
#' against a set of pathway memberships.  With a universe of N genes, a
#' pathway containing m of them, a selected list of n genes and q of
#' those in the pathway, the one-sided p-value is the upper
#' hypergeometric tail.  The tail sum is formed in log space through the
#' log-gamma function; p-values across pathways are adjusted by the
#' Benjamini-Hochberg step-up procedure.  Membership is passed as
#' indicator vectors over a fixed gene universe so that both language
#' arms index genes identically.
#'
#' Formula: P(X >= q) = sum_{j>=q} C(m,j) C(N-m, n-j) / C(N, n).
#'
#' @param genes Indicator over the gene universe: 1 if selected.
#' @param kegg_pathways Membership matrix, one row per gene of the
#'   universe, one column per pathway, entries 0 or 1.
#' @param alpha FDR level used for the reported decisions.
#' @return List with \code{estimate} (smallest p-value), \code{pvalue},
#'   \code{qvalue}, \code{overlap}, \code{pathway_size},
#'   \code{top_pathway}, \code{n_significant}, \code{significant},
#'   \code{n_selected}, \code{n_pathways}, \code{alpha}, \code{n},
#'   \code{method}.
#' @references Kanehisa, Furumichi, Tanabe, Sato and Morishima (2017),
#'   KEGG: new perspectives on genomes, pathways, diseases and drugs,
#'   Nucleic Acids Research 45(D1):D353-D361,
#'   \doi{10.1093/nar/gkw1092}; Benjamini and Hochberg (1995), JRSS-B
#'   57(1):289-300. \doi{10.1111/j.2517-6161.1995.tb02031.x}
#' @export
#' @examples
#' genes <- c(1, 0, 1, 0, 1)
#' kegg_pathways <- matrix(c(1, 1, 0, 1, 0, 0, 0, 1, 1, 0, 0, 1, 1, 1, 0),
#'                         5, 3, byrow = TRUE)
#' Keggp(genes, kegg_pathways)
Keggp <- function(genes, kegg_pathways, alpha = 0.05) {
  g <- .s03vec(genes); N <- length(g)
  if (N == 0L) stop("kegg_pathway: genes is empty")
  if (any(g != 0 & g != 1)) stop("kegg_pathway: genes must be a 0/1 indicator over the universe")
  P <- .s03mat(kegg_pathways)
  if (nrow(P) != N) stop("kegg_pathway: kegg_pathways must have one row per gene of the universe")
  K <- ncol(P)
  if (any(P != 0 & P != 1)) stop("kegg_pathway: kegg_pathways must be a 0/1 membership matrix")
  if (!(alpha > 0 && alpha < 1)) stop("kegg_pathway: alpha must lie in (0, 1)")
  nsel <- sum(g)
  if (nsel == 0) stop("kegg_pathway: no genes selected")
  lch <- function(a, b) if (b < 0 || b > a) -Inf else lgamma(a + 1) - lgamma(b + 1) - lgamma(a - b + 1)
  ptail <- function(q, m, N, n) {
    hi <- min(m, n)
    if (q <= max(0, n - (N - m))) return(1)
    if (q > hi) return(0)
    den <- lch(N, n)
    tot <- 0
    for (j in q:hi) tot <- tot + exp(lch(m, j) + lch(N - m, n - j) - den)
    min(tot, 1)
  }
  sizes <- numeric(K); ov <- numeric(K); pv <- numeric(K)
  for (cc in seq_len(K)) {
    m <- sum(P[, cc]); q <- sum(P[g == 1, cc])
    sizes[cc] <- m; ov[cc] <- q; pv[cc] <- ptail(q, m, N, nsel)
  }
  ord <- order(pv, seq_len(K))
  qv <- numeric(K); prev <- 1
  for (rk in seq(K, 1L)) {
    i <- ord[rk]
    val <- min(pv[i] * K / rk, prev, 1)
    qv[i] <- val; prev <- val
  }
  best <- ord[1]
  .t1_result(estimate = pv[best], pvalue = pv, qvalue = qv, overlap = ov,
             pathway_size = sizes, top_pathway = best - 1L,
             n_significant = sum(qv <= alpha),
             significant = as.numeric(qv <= alpha), n_selected = nsel,
             n_pathways = K, alpha = alpha, n = N,
             method = "hypergeometric P(X >= q) per pathway with Benjamini-Hochberg FDR, Kanehisa et al (2017)")
}
