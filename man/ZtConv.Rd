% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{ZtConv}
\alias{ZtConv}
\title{Eq (3.56): y = x * h => Y(z) = X(z) H(z).  Both sides computed}
\usage{
ZtConv(x, h, z)
}
\arguments{
\item{x}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{h}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{z}{Coerced to complex by the body, with \code{as.complex}.}
}
\description{
separately so the property is demonstrated, not assumed.
}
\value{
A list with \code{y}, \code{Y}, \code{XH}, \code{z}, \code{max_difference}, \code{holds}, \code{method}.
}
\keyword{internal}
\examples{
ZtConv(x = c(1, 2, 3, 4, 5, 6, 7, 8), h = 0.5, z = c(1, 2, 3, 4, 5, 6, 7, 8))
}
