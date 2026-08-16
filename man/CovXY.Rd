% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{CovXY}
\alias{CovXY}
\title{Eqs (3.21)-(3.22): C_xy and rho = C_xy / (sigma_x sigma_y)}
\usage{
CovXY(x, y, ddof = 0)
}
\arguments{
\item{x}{Passed to \code{.morie_rg_aslist}.}
\item{y}{Passed to \code{.morie_rg_aslist}.}
\item{ddof}{Coerced to integer by the body, with \code{as.integer}. Defaults to \code{0}.}
}
\description{
A step of the rangayyan_ch3 implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{covariance}, \code{correlation}, \code{sd_x}, \code{sd_y}, \code{mean_x}, \code{mean_y}, \code{n}, \code{ddof}, \code{method}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
CovXY(V, V)
}
