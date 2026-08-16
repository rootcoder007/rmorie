% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{Dgamma}
\alias{Dgamma}
\title{Dgamma}
\usage{
Dgamma(x, shape, rate = 1, log = FALSE)
}
\arguments{
\item{x}{See Usage.}
\item{shape}{See Usage.}
\item{rate}{Defaults to \code{1}.}
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
Dgamma(x = c(1, 2, 3, 4, 5, 6, 7, 8), shape = 5L)
}
