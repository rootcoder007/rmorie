% Generated from the module source by ledger/wave3/gen_rd_any.R --
% roxygen2 hangs on this package. Usage comes from args(), so it
% tracks the formals.
\name{morie_phacf3_tanimoto}
\alias{morie_phacf3_tanimoto}
\title{Tanimoto coefficient of two equal-length fingerprints}
\usage{
morie_phacf3_tanimoto(a, b)
}
\arguments{
\item{a}{The first fingerprint.}
\item{b}{The second fingerprint.}
}
\description{
On counts rather than bits it is the weighted form: shared over union, with the minimum as the intersection. Two empty fingerprints have no features in common and none apart, which is a zero over zero; it is reported as NaN rather than as the 1 that would call two blank molecules identical.
}
\value{
The Tanimoto coefficient.
}
\references{
  Gund, P. (1977) "Three-dimensional pharmacophoric pattern
    searching." Progress in Molecular and Subcellular Biology 5,
    117-143.
  Mason, J.S., Good, A.C. and Martin, E.J. (2001) "3-D pharmacophores
    in drug discovery." Current Pharmaceutical Design 7(7), 567-597.
  Mason, J.S., Morize, I., Menard, P.R., Cheney, D.L., Hulme, C. and
    Labaudiniere, R.F. (1999) "New 4-point pharmacophore method for
    molecular similarity and diversity applications." Journal of
    Medicinal Chemistry 42(17), 3251-3264.
  Tanimoto, T.T. (1958) "An elementary mathematical theory of
    classification and prediction." IBM internal report.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
morie_phacf3_tanimoto(V, V)
}
