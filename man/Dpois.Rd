% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{Dpois}
\alias{Dpois}
\title{Dpois}
\usage{
Dpois(x, lambda, log = FALSE)
}
\arguments{
\item{x}{See Usage.}
\item{lambda}{See Usage.}
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
Dpois(x = c(1, 2, 3, 4, 5, 6, 7, 8), lambda = 0.5)
}
