% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_pace}
\alias{morie_pace}
\title{morie_pace}
\usage{
morie_pace(Y, argvals, K)
}
\arguments{
\item{Y}{See Usage.}
\item{argvals}{See Usage.}
\item{K}{See Usage.}
}
\description{
Part of the pace_native implementation; see the file header for the source it follows.
}
\value{
A list with \code{estimate}, \code{se}, \code{n}, \code{method}.
}
\keyword{internal}
\examples{
morie_pace(Y = c(1, 2, 3, 4, 5, 6, 7, 8), argvals = c(1, 2, 3, 4, 5, 6, 7, 8), K = 5L)
}
