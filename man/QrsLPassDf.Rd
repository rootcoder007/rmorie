% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{QrsLPassDf}
\alias{QrsLPassDf}
\title{Eq (4.8): y(n) = 2 y(n-1) - y(n-2) + (1/32)[x(n) - 2 x(n-6) +}
\usage{
QrsLPassDf(x) 
}
\arguments{
\item{x}{A vector; its length is taken and its elements indexed.}
}
\description{
x(n-12)] Adds and one shift by 32 -- which is why it was chosen for real time.
}
\value{
A list with \code{y}, \code{delay}, \code{n}, \code{fsnote}, \code{method}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
QrsLPassDf(V)
}
