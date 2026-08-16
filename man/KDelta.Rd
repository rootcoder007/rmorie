% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{KDelta}
\alias{KDelta}
\title{Eq (3.34): delta(n) = 1 if n = 0, 0 otherwise.  Unlike eq (3.24) this}
\usage{
KDelta(n, shift = 0, amplitude = 1)
}
\arguments{
\item{n}{A vector; its length is taken.}
\item{shift}{Defaults to \code{0}.}
\item{amplitude}{Defaults to \code{1}.}
}
\description{
is an ordinary sequence, evaluable at the origin.
}
\value{
A list with \code{delta}, \code{n}, \code{shift}, \code{amplitude}, \code{method}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
KDelta(V)
}
