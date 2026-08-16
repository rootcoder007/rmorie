% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.sarima_seasonal_lift}
\alias{.sarima_seasonal_lift}
\title{.sarima_seasonal_lift}
\usage{
.sarima_seasonal_lift(c, s)
}
\arguments{
\item{c}{A vector; its length is taken and its elements indexed.}
\item{s}{Numeric; combined arithmetically in the body.}
}
\description{
A step of the sarima_native implementation. Called by \code{expand_polynomials}. See the file header for the source the module follows.
}
\value{
The value of \code{out}, as built in the body.
}
\keyword{internal}
\examples{
.sarima_seasonal_lift(c = c(1, 2, 3, 4, 5, 6, 7, 8), s = 5L)
}
