% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{QrsHPassDf}
\alias{QrsHPassDf}
\title{Eq (4.12): p(n) = x(n-16) - (1/32)[y(n-1) + x(n) - x(n-32)], where}
\usage{
QrsHPassDf(x)
}
\arguments{
\item{x}{A vector; its length is taken and its elements indexed.}
}
\description{
the bracketed group is exactly y(n) of eq (4.10), so the running-sum state is reused directly.
}
\value{
A list with \code{p}, \code{y}, \code{n}, \code{method}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
QrsHPassDf(V)
}
