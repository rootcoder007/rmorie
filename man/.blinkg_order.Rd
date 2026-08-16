% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.blinkg_order}
\alias{.blinkg_order}
\title{Markers sorted by p value, ties broken by index. The tie break}
\usage{
.blinkg_order(pv)
}
\arguments{
\item{pv}{A vector; indexed elementwise.}
}
\description{
matters: on a small panel several markers can share a p value to the last bit, and a selection that depended on which one the sort happened to put first would not be reproducible.
}
\value{
The value of \code{[}.
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
.blinkg_order(V)
}
