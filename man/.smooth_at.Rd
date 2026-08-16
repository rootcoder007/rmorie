% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.smooth_at}
\alias{.smooth_at}
\title{.smooth_at}
\usage{
.smooth_at(xv, av, g, h, fit)
}
\arguments{
\item{xv}{A vector; its length is taken.}
\item{av}{Numeric; combined arithmetically in the body.}
\item{g}{Numeric; combined arithmetically in the body.}
\item{h}{Numeric; combined arithmetically in the body.}
\item{fit}{Compared against \code{"kernel"}.}
}
\description{
A step of the tmlcps_native implementation. Called by \code{.cv_bandwidth}, \code{effect_curve}. See the file header for the source the module follows.
}
\value{
A numeric value.
}
\keyword{internal}
