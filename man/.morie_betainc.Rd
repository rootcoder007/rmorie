% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_betainc}
\alias{.morie_betainc}
\title{Regularized incomplete beta I_x(a, b)}
\usage{
.morie_betainc(a, b, x)
}
\arguments{
\item{a}{Numeric; combined arithmetically in the body.}
\item{b}{Numeric; combined arithmetically in the body.}
\item{x}{Numeric; passed to \code{log}.}
}
\description{
A step of the dist_native implementation. Called by \code{Pbeta}, \code{Pbinom}, \code{Pf} and 1 others in the module. See the file header for the source the module follows.
}
\value{
One of two values, depending on the branch taken.
}
\keyword{internal}
\examples{
.morie_betainc(a = c(1, 2, 3, 4, 5, 6, 7, 8), b = 5L, x = 5L)
}
