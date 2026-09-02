% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{Pbinom}
\alias{Pbinom}
\title{P(X <= k) = I_{1-p}(n - k, k + 1)}
\usage{
Pbinom(q, size, prob, lower_tail = TRUE) 
}
\arguments{
\item{q}{Iterated over elementwise, with \code{vapply}.}
\item{size}{Numeric; combined arithmetically in the body.}
\item{prob}{Numeric; combined arithmetically in the body.}
\item{lower_tail}{A flag; the body branches on it. Defaults to \code{TRUE}.}
}
\description{
A step of the dist_native implementation. Called by \code{Qbinom}. See the file header for the source the module follows.
}
\value{
One of two values, depending on the branch taken.
}
\keyword{internal}
\examples{
Pbinom(q = 0.5, size = 5L, prob = 0.5)
}
