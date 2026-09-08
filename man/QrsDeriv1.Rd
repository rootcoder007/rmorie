% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{QrsDeriv1}
\alias{QrsDeriv1}
\title{Eq (4.1): y0(n) = |x(n) - x(n-2)|.  The two-sample span is what makes}
\usage{
QrsDeriv1(x) 
}
\arguments{
\item{x}{A vector; its length is taken and its elements indexed.}
}
\description{
it "smoothed"; the absolute value makes an inverted QRS give the same response.
}
\value{
A list with \code{y0}, \code{n}, \code{method}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
QrsDeriv1(V)
}
