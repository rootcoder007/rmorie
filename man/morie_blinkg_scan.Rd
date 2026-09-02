% Generated from the module source by ledger/wave3/gen_rd_any.R --
% roxygen2 hangs on this package. Usage comes from args(), so it
% tracks the formals.
\name{morie_blinkg_scan}
\alias{morie_blinkg_scan}
\title{Test every marker with the current pseudo-QTNs as covariates}
\usage{
morie_blinkg_scan(y, geno, covars = NULL, qtn = integer(0)) 
}
\arguments{
\item{y}{The phenotype.}
\item{geno}{A list of marker vectors, one value per individual.}
\item{covars}{A list of fixed covariate vectors, or NULL.}
\item{qtn}{Indices of the current pseudo-QTNs, one-based.}
}
\description{
A marker with no variation, or one whose design is rank deficient once the covariates are in, gets a p value of NaN rather than a fabricated number.
}
\value{
A list with the effect, its standard error, the t statistic   and the two sided p value for each marker.
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
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
morie_blinkg_scan(V, V)
}
