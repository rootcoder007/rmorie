% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{ContProj}
\alias{ContProj}
\title{Eq (4.26): the continuous counterpart of eq (4.24).  Tabulated it is}
\usage{
ContProj(x, y, t = NULL, dt = 1)
}
\arguments{
\item{x}{See Usage.}
\item{y}{See Usage.}
\item{t}{Defaults to \code{NULL}.}
\item{dt}{Defaults to \code{1}.}
}
\description{
the discrete inner product SCALED BY dt; dropping the dt turns an integral into a sum and leaves the answer wrong by a factor of the sampling interval.
}
\value{
A list with \code{theta}, \code{integrand}, \code{discrete_sum}, \code{duration}, \code{n}, \code{method}.
}
\keyword{internal}
