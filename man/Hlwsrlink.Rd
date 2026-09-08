% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{Hlwsrlink}
\alias{Hlwsrlink}
\title{Hodges-Lehmann estimator and Walsh counting identity -- pp. 209-210}
\usage{
Hlwsrlink(x, m0 = 0) 
}
\description{
Hodges-Lehmann estimator and Walsh counting identity -- pp. 209-210
}
\arguments{
\item{x}{Argument \code{x}; see Usage.}
\item{m0}{Argument \code{m0}; see Usage.}
}
\value{
A list with \code{estimate}, \code{tplus}, \code{tminus}, \code{nbelow}, \code{nequal}, \code{nabove}, \code{nwalsh}, \code{n}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
rmorie:::Hlwsrlink(V)
}
