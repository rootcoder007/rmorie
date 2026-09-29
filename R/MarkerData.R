#' Marker heterozygosity and min-max normalisation
#'
#' R arm of \code{morie.fn.hetlc} and \code{mmaxn}. \code{Hetlc}: per-marker
#' observed heterozygosity, expected heterozygosity 2p(1 - p) and per-
#' individual heterozygous-locus frequency for 0/1/2 genotypes (NA skipped).
#' \code{Mmaxn}: min-max normalisation onto the unit interval, column-wise for
#' matrices.
#'
#' @param marker_matrix Genotypes coded 0, 1, 2 (individuals by markers).
#' @param x Vector or matrix.
#' @return A named list (the Python result's fields).
#' @references Nei, M. (1973). Analysis of gene diversity in subdivided populations. PNAS 70, 3321-3323.
#'
#'   Montesinos Lopez, O. A., Montesinos Lopez, A. and Crossa, J. (2022). Multivariate Statistical Machine Learning Methods for Genomic Prediction. Springer.
#' @examples
#' Hetlc(rbind(c(0, 1, 2), c(1, 1, 2), c(2, 0, 1)))$H_exp
#' Mmaxn(c(2, 4, 3, 10))$x_norm
#' @export
Hetlc <- function(marker_matrix) {
  M <- as.matrix(marker_matrix)
  if (any(!is.na(M) & !(M %in% c(0, 1, 2)))) stop("genotypes must be coded 0, 1 or 2")
  p <- colSums(M, na.rm = TRUE) / (2 * colSums(!is.na(M)))
  hexp <- 2 * p * (1 - p)
  hobs <- colSums(M == 1, na.rm = TRUE) / colSums(!is.na(M))
  list(H_obs = unname(hobs), H_exp = unname(hexp), allele_freq = unname(p),
       het_freq_individual = unname(rowSums(M == 1, na.rm = TRUE) / rowSums(!is.na(M))),
       estimate = mean(hexp), n = nrow(M), m = ncol(M))
}

#' @rdname Hetlc
#' @export
Mmaxn <- function(x) {
  f <- function(v) if (max(v) > min(v)) (v - min(v)) / (max(v) - min(v)) else rep(0, length(v))
  if (is.matrix(x)) {
    return(list(x_norm = apply(x, 2, f), min = apply(x, 2, min), max = apply(x, 2, max)))
  }
  list(x_norm = f(x), min = min(x), max = max(x))
}
