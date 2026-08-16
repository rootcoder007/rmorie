% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{FDiffTf}
\alias{FDiffTf}
\title{Eq (3.124): one zero, at z = 1, the DC point -- that single zero is}
\usage{
FDiffTf(z, T = 1)
}
\arguments{
\item{z}{Passed to \code{.morie_rg_polyz}.}
\item{T}{Defaults to \code{1}.}
}
\description{
the whole of the operator\'s highpass character.
}
\value{
A list with \code{H}, \code{z}, \code{T}, \code{zeros}, \code{zero_at_dc}, \code{dc_gain}, \code{method}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
FDiffTf(V)
}
