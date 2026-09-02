% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{Ppois}
\alias{Ppois}
\title{P(X <= k) = Q(k+1, lambda), the UPPER regularized incomplete gamma}
\usage{
Ppois(q, lambda, lower_tail = TRUE) 
}
\arguments{
\item{q}{Iterated over elementwise, with \code{vapply}.}
\item{lambda}{Passed to \code{.morie_gammainc_p}.}
\item{lower_tail}{A flag; the body branches on it. Defaults to \code{TRUE}.}
}
\description{
A step of the dist_native implementation. Called by \code{Qpois}. See the file header for the source the module follows.
}
\value{
One of two values, depending on the branch taken.
}
\keyword{internal}
\examples{
Ppois(q = 0.5, lambda = 0.5)
}
