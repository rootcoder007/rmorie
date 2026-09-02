% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{QrsDeriv2}
\alias{QrsDeriv2}
\title{Eq (4.2): y1(n) = |x(n) - 2 x(n-2) + x(n-4)|.  The second difference}
\usage{
QrsDeriv2(x) 
}
\arguments{
\item{x}{A vector; its length is taken and its elements indexed.}
}
\description{
removes any locally linear trend, so T-wave limbs and baseline drift give nothing while the QRS curvature gives a large output.
}
\value{
A list with \code{y1}, \code{n}, \code{method}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
QrsDeriv2(V)
}
