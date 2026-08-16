% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{Twiddle}
\alias{Twiddle}
\title{Eq (3.82): W_N = exp(-j 2 pi / N), the N-th root of unity}
\usage{
Twiddle(npoints, power = 1)
}
\arguments{
\item{npoints}{See Usage.}
\item{power}{Defaults to \code{1}.}
}
\description{
Part of the rangayyan_xfrm implementation; see the file header for the source it follows.
}
\value{
A list with \code{W}, \code{N}, \code{power}, \code{root_of_unity}, \code{method}.
}
\keyword{internal}
\examples{
Twiddle(npoints = 5L)
}
