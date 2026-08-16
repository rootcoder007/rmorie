% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.blinkg_design}
\alias{.blinkg_design}
\title{Intercept, fixed covariates, then the given genotype columns}
\usage{
.blinkg_design(n, covars, cols)
}
\arguments{
\item{n}{A count; the body uses it as \code{matrix(...)}.}
\item{covars}{A vector; its length is taken.}
\item{cols}{A vector; its length is taken.}
}
\description{
A step of the blinkg_native implementation. Called by \code{morie_blinkg_scan}, \code{morie_blinkg_select}. See the file header for the source the module follows.
}
\value{
The value of \code{d}, as built in the body.
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
