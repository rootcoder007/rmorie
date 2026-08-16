% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{Dexp}
\alias{Dexp}
\title{Dexp}
\usage{
Dexp(x, rate = 1, log = FALSE)
}
\arguments{
\item{x}{Numeric; combined arithmetically in the body.}
\item{rate}{Numeric; passed to \code{log}. Defaults to \code{1}.}
\item{log}{A flag; the body branches on it. Defaults to \code{FALSE}.}
}
\description{
A step of the dist_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
One of two values, depending on the branch taken.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
Dexp(V)
}
