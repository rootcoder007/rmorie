# Linkage disequilibrium between two biallelic loci.
# Sources: Lewontin, R. C. (1964), The interaction of selection and
# linkage. I. General considerations; heterotic models, Genetics
# 49(1), 49-67 (the disequilibrium coefficient D = p_AB - p_A p_B);
# Hill, W. G. and Robertson, A. (1968), Linkage disequilibrium in
# finite populations, Theoretical and Applied Genetics 38, 226-231
# (the squared correlation r^2 = D^2 / [p_A(1-p_A) p_B(1-p_B)]);
# Purcell, S. et al. (2007), PLINK, American Journal of Human
# Genetics 81(3), 559-575, Sec. "Linkage disequilibrium".
#
# Native implementation mirroring Python morie.fn.ld, including the
# degenerate case: when either locus is monomorphic the denominator is
# zero and r^2 is reported as 0 rather than NaN.

#' Linkage disequilibrium (r-squared) between two loci
#'
#' For two 0/1-coded loci observed on the same individuals, returns
#' the squared correlation
#' \eqn{r^2 = D^2 / [p_A(1-p_A)p_B(1-p_B)]} with
#' \eqn{D = p_{AB} - p_A p_B} (Lewontin 1964; Hill and Robertson
#' 1968).  \eqn{r^2} is exactly the squared Pearson correlation of the
#' two indicator vectors, which is the form used for LD pruning.
#'
#' @param locus_a,locus_b Equal-length vectors containing only 0 and 1.
#' @return A list with \code{name}, \code{statistic} (\eqn{r^2}),
#'   \code{n}, and \code{extra} holding \code{D}, \code{pA},
#'   \code{pB}.
#' @references Hill, W. G. and Robertson, A. (1968). Linkage
#'   disequilibrium in finite populations. Theoretical and Applied
#'   Genetics, 38, 226-231.
#' @export
morie_ld <- function(locus_a, locus_b) {
  a <- as.numeric(locus_a); b <- as.numeric(locus_b)
  if (length(a) != length(b)) stop("Locus arrays must have the same length.")
  if (length(a) == 0L) stop("Arrays must not be empty.")
  if (!all(a %in% c(0, 1))) stop("locus_a must contain only 0 and 1.")
  if (!all(b %in% c(0, 1))) stop("locus_b must contain only 0 and 1.")
  n <- length(a)
  pA <- mean(a); pB <- mean(b)
  denom <- pA * (1 - pA) * pB * (1 - pB)
  if (denom == 0)
    return(list(name = "LD_r2", statistic = 0, n = n,
                extra = list(D = 0, pA = pA, pB = pB)))
  D <- mean(a * b) - pA * pB
  list(name = "LD_r2", statistic = (D * D) / denom, n = n,
       extra = list(D = D, pA = pA, pB = pB))
}
