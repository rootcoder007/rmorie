% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{QrsDerivOp}
\alias{QrsDerivOp}
\title{Eq (4.14): y(n) = (1/8)[2 x(n) + x(n-1) - x(n-3) - 2 x(n-4)].  The}
\usage{
QrsDerivOp(x)
}
\arguments{
\item{x}{See Usage.}
}
\description{
antisymmetric taps make it exactly zero on any constant or linear baseline.
}
\value{
A list with \code{y}, \code{b}, \code{n}, \code{fsnote}, \code{method}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
QrsDerivOp(V)
}
