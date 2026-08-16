% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{CLogProd}
\alias{CLogProd}
\title{CLogProd: complex log turning Y = X H into log X + log H;}
\usage{
CLogProd(X, H, omega = NULL)
}
\arguments{
\item{X}{See Usage.}
\item{H}{See Usage.}
\item{omega}{Defaults to \code{NULL}.}
}
\description{
Rangayyan (2024) eq. (4.63), Section 4.8.
}
\value{
A list with \code{omega}, \code{Y_real}, \code{Y_imag}, \code{log_Y_real}, \code{log_Y_imag}, \code{log_X_real}, \code{log_X_imag}, \code{log_H_real}, \code{log_H_imag}, \code{max_abs_error}, \code{units}, \code{method}.
}
\keyword{internal}
