% Generated from the module source by ledger/wave3/gen_rd_any.R --
% roxygen2 hangs on this package. Usage comes from args(), so it
% tracks the formals.
\name{morie_blinkg}
\alias{morie_blinkg}
\title{Iterate the scan and the pseudo-QTN selection to a fixed point}
\usage{
morie_blinkg(y, geno, positions = NULL, covars = NULL, selection = "ld", criterion = "bic", ld_threshold = .BLINKG_LD_THRESHOLD, alpha = .BLINKG_ALPHA, bin_size = NULL, max_iter = 10L)
}
\arguments{
\item{y}{The phenotype.}
\item{geno}{A list of marker vectors, one value per individual.}
\item{positions}{Genomic positions, needed only by the bin selection.}
\item{covars}{Fixed covariates -- principal components, say -- carried   in every model, or NULL.}
\item{selection}{A member of the selection list.}
\item{criterion}{A member of the criterion list.}
\item{ld_threshold}{Correlation above which a candidate is dropped.}
\item{alpha}{The Bonferroni level for the pre-filter.}
\item{bin_size}{Bin width for the bin selection.}
\item{max_iter}{Iteration cap. Reaching it is reported, not hidden.}
}
\description{
Iterate the scan and the pseudo-QTN selection to a fixed point
}
\value{
A list with the final scan, the pseudo-QTNs and how they were   chosen, the criterion path, the iteration count and whether the set   settled, and the genomic inflation factor.
}
\references{
  Huang, M., Liu, X., Zhou, Y., Summers, R.M. and Zhang, Z. (2019)
    "BLINK: a package for the next level of genome-wide association
    studies with both individuals and markers in the millions."
    GigaScience 8(2), giy154. doi:10.1093/gigascience/giy154. The LD
    filter with its 0.7 threshold and the Bonferroni pre-filter at
    alpha = 0.01, the BIC = 2(-LL) + k log n selection, and the
    iteration to a stable pseudo-QTN set.
  Liu, X., Huang, M., Fan, B., Buckler, E.S. and Zhang, Z. (2016)
    "Iterative usage of fixed and random effect models for powerful
    and efficient genome-wide association studies." PLoS Genetics
    12(2), e1005767. FarmCPU, the bin-based predecessor.
  Devlin, B. and Roeder, K. (1999) "Genomic control for association
    studies." Biometrics 55(4), 997-1004.
}
\keyword{internal}
\examples{
N <- 60L
M <- 25L
G <- lapply(0:(M - 1L), function(j) vapply(0:(N - 1L), function(i) floor((((i + 
    1) * (j + 1) * 2654435761)\%\%2147483648)/8192)\%\%3, numeric(1)))
Y <- vapply(0:(N - 1L), function(i) 2 * G[[6]][i + 1L] + 0.7 * 
    G[[13]][i + 1L] + (((i * 17)\%\%11) - 5)/10, numeric(1))
morie_blinkg(Y, G)
}
