% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{Pexp}
\alias{Pexp}
\title{Pexp}
\usage{
Pexp(q, rate = 1, lower_tail = TRUE) 
}
\arguments{
\item{q}{Numeric; combined arithmetically in the body.}
\item{rate}{Numeric; combined arithmetically in the body. Defaults to \code{1}.}
\item{lower_tail}{A flag; the body branches on it. Defaults to \code{TRUE}.}
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
Pexp(V)
}
