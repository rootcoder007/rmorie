% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.schab_whittle_covariance}
\alias{.schab_whittle_covariance}
\title{.schab_whittle_covariance}
\usage{
.schab_whittle_covariance(h, sigma2 = 1, theta = 1)
}
\arguments{
\item{h}{Numeric; combined arithmetically in the body.}
\item{sigma2}{A count; the body uses it as \code{rep(...)}. Defaults to \code{1}.}
\item{theta}{Numeric; combined arithmetically in the body. Defaults to \code{1}.}
}
\description{
A step of the schab_st_shared implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
The value of \code{out}, as built in the body.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.schab_whittle_covariance(V)
}
