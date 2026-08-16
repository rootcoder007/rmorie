% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{rND}
\alias{rND}
\title{Normalised discounted difference}
\usage{
rND(protected, step = 10, normalize = TRUE)
}
\arguments{
\item{protected}{Passed to \code{.measure}.}
\item{step}{Passed to \code{.measure}. Defaults to \code{10}.}
\item{normalize}{Passed to \code{.measure}. Defaults to \code{TRUE}.}
}
\description{
A step of the fairRC_native implementation. Called by \code{morie_fairRC}. See the file header for the source the module follows.
}
\value{
The value of \code{.measure}.
}
\keyword{internal}
