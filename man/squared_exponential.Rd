% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{squared_exponential}
\alias{squared_exponential}
\title{squared_exponential}
\usage{
squared_exponential(a, b, amplitude = 1, length_scale = 1)
}
\arguments{
\item{a}{A vector; its length is taken.}
\item{b}{Passed to \code{.r2}.}
\item{amplitude}{Numeric; combined arithmetically in the body. Defaults to \code{1}.}
\item{length_scale}{Passed to \code{.lengths}. Defaults to \code{1}.}
}
\description{
A step of the bayopt_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A numeric value.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
squared_exponential(V, V)
}
