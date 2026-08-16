% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_didfst_did_estimate}
\alias{morie_didfst_did_estimate}
\title{The DiD contrast of eq. (13.7) under arbitrary unit weights}
\usage{
morie_didfst_did_estimate(delta, D, weights = NULL)
}
\arguments{
\item{delta}{Numeric vector of differences.}
\item{D}{Numeric vector of 0/1 adoption indicators.}
\item{weights}{Optional numeric vector of unit weights.}
}
\description{
With \code{weights = NULL} every unit counts equally and this is the textbook estimator. Passing forest weights turns it into the local estimate at a point.
}
\value{
A list with \code{estimate}, \code{treated_mean},   \code{control_mean}, \code{treated_weight}, \code{control_weight}.
}
\keyword{internal}
