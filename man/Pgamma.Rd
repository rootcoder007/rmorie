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
\item{shape}{Passed to \code{.morie_gammainc_p}.}
\item{rate}{Numeric; combined arithmetically in the body. Defaults to \code{1}.}
\item{lower_tail}{A flag; the body branches on it. Defaults to \code{TRUE}.}
}
\description{
A step of the dist_native implementation. Called by \code{Pchisq}, \code{Qgamma}. See the file header for the source the module follows.
}
\value{
One of two values, depending on the branch taken.
}
\keyword{internal}
\examples{
Pgamma(q = 0.5, shape = 5L)
}
