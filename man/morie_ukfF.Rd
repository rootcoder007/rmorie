% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_ukfF}
\alias{morie_ukfF}
\title{morie_ukfF}
\usage{
morie_ukfF(f, h, Q, R, x0, P0, measurements, kappa = NULL)
}
\arguments{
\item{f}{See Usage.}
\item{h}{See Usage.}
\item{Q}{See Usage.}
\item{R}{See Usage.}
\item{x0}{See Usage.}
\item{P0}{See Usage.}
\item{measurements}{See Usage.}
\item{kappa}{Defaults to \code{NULL}.}
}
\description{
Part of the ukfF_mixedcase_native implementation; see the file header for the source it follows.
}
\value{
A list with \code{states}, \code{covariances}, \code{innovations}, \code{kappa}, \code{method}.
}
\keyword{internal}
