% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.schab_st_jones_zhang}
\alias{.schab_st_jones_zhang}
\title{.schab_st_jones_zhang}
\usage{
.schab_st_jones_zhang(h, k, sigma2 = 1, theta = 1, c = 1, p = 1.5, d = 2, n_quad = 40L)
}
\arguments{
\item{h}{Passed to \code{.schab_st_as_lags}.}
\item{k}{Passed to \code{.schab_st_as_lags}.}
\item{sigma2}{Numeric; combined arithmetically in the body. Defaults to \code{1}.}
\item{theta}{Passed to \code{.schab_st_hankel_panels}. Defaults to \code{1}.}
\item{c}{Numeric; combined arithmetically in the body. Defaults to \code{1}.}
\item{p}{Passed to \code{.schab_st_hankel_panels}. Defaults to \code{1.5}.}
\item{d}{Numeric; combined arithmetically in the body. Defaults to \code{2}.}
\item{n_quad}{Passed to \code{.schab_st_hankel_panels}. Defaults to \code{40L}.}
}
\description{
A step of the schab_st_shared implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{covariance}, \code{quadrature}.
}
\keyword{internal}
