% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.measure}
\alias{.measure}
\title{.measure}
\usage{
.measure(protected, measure, step, normalize, caveat = NULL)
}
\arguments{
\item{protected}{Coerced to logical by the body, with \code{as.logical}.}
\item{measure}{Passed to \code{.raw}.}
\item{step}{Passed to \code{.raw}.}
\item{normalize}{A flag; the body branches on it.}
\item{caveat}{Defaults to \code{NULL}.}
}
\description{
A step of the fairRC_native implementation. Called by \code{rKL}, \code{rND}, \code{rRD}. See the file header for the source the module follows.
}
\value{
The value of \code{pay}, as built in the body.
}
\keyword{internal}
