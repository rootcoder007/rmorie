% Generated from the module source by ledger/wave3/gen_rd_any.R --
% roxygen2 hangs on this package. Usage comes from args(), so it
% tracks the formals.
\name{morie_blinkg_select}
\alias{morie_blinkg_select}
\title{Choose how many of the ranked candidates to keep}
\usage{
morie_blinkg_select(y, geno, candidates, covars = NULL, criterion = "bic")
}
\arguments{
\item{y}{The phenotype.}
\item{geno}{A list of marker vectors.}
\item{candidates}{Ranked candidate marker indices.}
\item{covars}{A list of fixed covariate vectors, or NULL.}
\item{criterion}{A member of the criterion list.}
}
\description{
Fits the first k of them for k from one to all, scores each fit and takes the best. The criterion counts the pseudo-QTNs, as the paper writes it, and not the intercept or the fixed covariates -- those are in every model being compared, so they cannot separate them.
}
\value{
A list with the chosen indices, the score path and the chosen   count.
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
morie_blinkg_select(y = c(1, 2, 3, 4, 5, 6, 7, 8), geno = c(1, 2, 3, 4, 5, 6, 7, 8), candidates = c(1, 2, 3, 4, 5, 6, 7, 8))
}
