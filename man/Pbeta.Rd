% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{Pbeta}
\alias{Pbeta}
\title{Pbeta}
\usage{
Pbeta(q, shape1, shape2, lower_tail = TRUE)
}
\arguments{
\item{q}{Iterated over elementwise, with \code{vapply}.}
\item{shape1}{Passed to \code{.morie_betainc}.}
\item{shape2}{Passed to \code{.morie_betainc}.}
\item{lower_tail}{A flag; the body branches on it. Defaults to \code{TRUE}.}
}
\description{
A step of the dist_native implementation. Called by \code{Qbeta}. See the file header for the source the module follows.
}
\value{
One of two values, depending on the branch taken.
}
\keyword{internal}
\examples{
Pbeta(q = 1, shape1 = c(1, 2, 3, 4, 5, 6, 7, 8), shape2 = c(1, 2, 3, 4, 5, 6, 7, 8))
}
