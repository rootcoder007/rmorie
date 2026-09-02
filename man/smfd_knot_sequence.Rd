% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{smfd_knot_sequence}
\alias{smfd_knot_sequence}
\title{Internal: evenly spaced knots, extended by \'degree\' at each end}
\usage{
smfd_knot_sequence(xmin, xmax, nseg = 10L, degree = 3L) 
}
\arguments{
\item{xmin}{Numeric; combined arithmetically in the body.}
\item{xmax}{Numeric; combined arithmetically in the body.}
\item{nseg}{Numeric; combined arithmetically in the body. Defaults to \code{10L}.}
\item{degree}{Numeric; combined arithmetically in the body. Defaults to \code{3L}.}
}
\description{
Mirrors Python knot_sequence(xmin, xmax, nseg, degree) in smfd.
}
\value{
A numeric value.
}
\keyword{internal}
\examples{
smfd_knot_sequence(0, 1, nseg = 10L, degree = 3L)
}
