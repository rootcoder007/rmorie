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
\item{x}{See Usage.}
\item{y}{See Usage.}
\item{ddof}{Defaults to \code{0}.}
}
\description{
Part of the rangayyan_ch3 implementation; see the file header for the source it follows.
}
\value{
A list with \code{covariance}, \code{correlation}, \code{sd_x}, \code{sd_y}, \code{mean_x}, \code{mean_y}, \code{n}, \code{ddof}, \code{method}.
}
\keyword{internal}
