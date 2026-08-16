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
\item{q}{See Usage.}
\item{lambda}{See Usage.}
\item{lower_tail}{Defaults to \code{TRUE}.}
}
\description{
Part of the dist_native implementation; see the file header for the source it follows.
}
\value{
One of two values, depending on the branch taken.
}
\keyword{internal}
