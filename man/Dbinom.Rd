% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{Dbinom}
\alias{Dbinom}
\title{Dbinom}
\usage{
Dbinom(x, size, prob, log = FALSE)
}
\arguments{
\item{x}{See Usage.}
\item{size}{See Usage.}
\item{prob}{See Usage.}
\item{log}{Defaults to \code{FALSE}.}
}
\description{
Part of the dist_native implementation; see the file header for the source it follows.
}
\value{
One of two values, depending on the branch taken.
}
\keyword{internal}
\examples{
Dbinom(x = c(1, 2, 3, 4, 5, 6, 7, 8), size = 5L, prob = 0.5)
}
