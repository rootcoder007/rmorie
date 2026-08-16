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
\item{f}{Passed to \code{.ukfF_ut}.}
\item{h}{Passed to \code{.ukfF_ut}.}
\item{Q}{See Usage.}
\item{R}{A matrix; passed to \code{as.matrix}.}
\item{x0}{See Usage.}
\item{P0}{See Usage.}
\item{measurements}{See Usage.}
\item{kappa}{Optional; may be \code{NULL}. Numeric; combined arithmetically in the body.}
}
\description{
A step of the ukfF_mixedcase_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{states}, \code{covariances}, \code{innovations}, \code{kappa}, \code{method}.
}
\keyword{internal}
