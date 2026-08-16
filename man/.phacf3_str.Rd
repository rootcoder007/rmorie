% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.phacf3_str}
\alias{.phacf3_str}
\title{A key\\'s string form, zero padded so that byte order is numeric order}
\usage{
.phacf3_str(k)
}
\arguments{
\item{k}{A vector; indexed elementwise.}
}
\description{
Only used to index the lookup table; the ORDERING of the bit space is done on the numbers themselves.
}
\value{
A character value.
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
.phacf3_str(V)
}
