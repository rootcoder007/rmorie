% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{Pgamma}
\alias{Pgamma}
\title{Pgamma}
\usage{
Pgamma(q, shape, rate = 1, lower_tail = TRUE)
}
\arguments{
\item{q}{See Usage.}
\item{shape}{See Usage.}
\item{rate}{Defaults to \code{1}.}
\item{lower_tail}{Defaults to \code{TRUE}.}
}
\description{
Part of the dist_native implementation; see the file header for the source it follows.
}
\value{
One of two values, depending on the branch taken.
}
\keyword{internal}
\examples{
Pgamma(q = 0.5, shape = 5L)
}
