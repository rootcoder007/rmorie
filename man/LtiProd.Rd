% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{LtiProd}
\alias{LtiProd}
\title{Eqs (3.50), (3.53): convolution in time is multiplication in the s}
\usage{
LtiProd(x, h, s = NULL, omega = NULL, dt = 1)
}
\arguments{
\item{x}{See Usage.}
\item{h}{See Usage.}
\item{s}{Defaults to \code{NULL}.}
\item{omega}{Defaults to \code{NULL}.}
\item{dt}{Defaults to \code{1}.}
}
\description{
and omega domains.  s = j omega recovers the frequency-domain form, so one function covers both.
}
\value{
A list with \code{y}, \code{Y}, \code{X}, \code{H}, \code{XH}, \code{s}, \code{max_difference}, \code{holds}, \code{method}.
}
\keyword{internal}
