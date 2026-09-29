#' Minor allele frequency of each SNP in a marker matrix
#'
#' With genotypes coded as allele counts (0, 1, 2), the allele frequency at
#' locus \eqn{j} is \eqn{p_j = \sum_i x_{ij} / (2 n_j)} over the \eqn{n_j}
#' genotyped individuals and the minor allele frequency is
#' \eqn{\min(p_j, 1 - p_j)}. Missing genotypes are skipped locus by locus.
#' Identical to the Python arm \code{morie.fn.mafcl.maf_calculation}.
#'
#' @param marker_matrix Individuals by loci genotype matrix (a vector is one
#'   locus).
#' @param coding \code{"012"} allele counts, or \code{"-101"} for the centred
#'   coding used by BGLR (shifted by one first).
#' @return A list with \code{maf}, \code{p}, \code{n_genotyped},
#'   \code{estimate} (mean MAF over loci), \code{n}, \code{n_loci} and
#'   \code{method}.
#' @references Montesinos Lopez, O. A., Montesinos Lopez, A. and Crossa, J.
#'   (2022). Multivariate Statistical Machine Learning Methods for Genomic
#'   Prediction. Springer, chapter 2.
#' @examples
#' maf_calculation(rbind(c(0, 2), c(1, 2), c(2, 1), c(0, 2)))$maf
#' @export
maf_calculation <- function(marker_matrix, coding = "012") {
  if (!coding %in% c("012", "-101")) stop("coding must be '012' or '-101'")
  shift <- if (coding == "-101") 1 else 0
  M <- if (is.null(dim(marker_matrix))) matrix(as.numeric(marker_matrix), ncol = 1) else as.matrix(marker_matrix)
  storage.mode(M) <- "double"
  if (length(M) == 0) stop("marker_matrix is empty")
  L <- ncol(M)
  maf <- p <- ng <- numeric(L)
  for (j in seq_len(L)) {
    g <- M[!is.na(M[, j]), j] + shift
    if (!length(g)) stop(sprintf("locus %d has no genotyped individuals", j - 1))
    if (!all(g %in% c(0, 1, 2))) stop(sprintf("a genotype at locus %d is not a valid %s code", j - 1, coding))
    p[j] <- sum(g) / (2 * length(g))
    maf[j] <- min(p[j], 1 - p[j])
    ng[j] <- length(g)
  }
  list(maf = maf, p = p, n_genotyped = ng, estimate = sum(maf) / L, n = nrow(M),
       n_loci = L, method = "Minor allele frequency min(p, 1 - p), p = allele count / (2 n)")
}
