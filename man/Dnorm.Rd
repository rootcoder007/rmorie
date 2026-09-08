% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{Dnorm}
\alias{Dnorm}
\title{Dnorm}
\usage{
Dnorm(x, mean = 0, sd = 1, log = FALSE) 
}
\arguments{
\item{x}{Numeric; combined arithmetically in the body.}
\item{mean}{Numeric; combined arithmetically in the body. Defaults to \code{0}.}
\item{sd}{Numeric; passed to \code{log}. Defaults to \code{1}.}
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
Dnorm(V)
}
