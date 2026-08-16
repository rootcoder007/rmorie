% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{QrsHpLpDf}
\alias{QrsHpLpDf}
\title{Eq (4.10): y(n) = y(n-1) + x(n) - x(n-32).  One add and one subtract}
\usage{
QrsHpLpDf(x)
}
\arguments{
\item{x}{A vector; its length is taken and its elements indexed.}
}
\description{
per sample regardless of window length.
}
\value{
A list with \code{y}, \code{n}, \code{method}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
QrsHpLpDf(V)
}
