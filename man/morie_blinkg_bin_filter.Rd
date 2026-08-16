% Generated from the module source by ledger/wave3/gen_rd_any.R --
% roxygen2 hangs on this package. Usage comes from args(), so it
% tracks the formals.
\name{morie_blinkg_bin_filter}
\alias{morie_blinkg_bin_filter}
\title{One marker per genomic bin, the most significant in the bin}
\usage{
morie_blinkg_bin_filter(order, positions, bin_size)
}
\arguments{
\item{order}{Marker indices in p-value order.}
\item{positions}{Genomic positions, one per marker.}
\item{bin_size}{Bin width.}
}
\description{
FarmCPU's rule. It cannot drop a distant marker that correlates by chance, and it cannot keep two real signals that fall in one bin -- which is the trade the LD filter is making.
}
\value{
The kept marker indices, most significant first.
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
