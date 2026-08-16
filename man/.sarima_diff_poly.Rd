% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.sarima_diff_poly}
\alias{.sarima_diff_poly}
\title{.sarima_diff_poly}
\usage{
.sarima_diff_poly(k, s)
}
\arguments{
\item{k}{Coerced to integer by the body, with \code{as.integer}.}
\item{s}{Numeric; combined arithmetically in the body.}
}
\description{
A step of the sarima_native implementation. Called by \code{forecast}. See the file header for the source the module follows.
}
\value{
The value of \code{out}, as built in the body.
}
\keyword{internal}
\examples{
.sarima_diff_poly(k = 5L, s = 5L)
}
