% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.plcbsc_gaps}
\alias{.plcbsc_gaps}
\title{.plcbsc_gaps}
\usage{
.plcbsc_gaps(y_treated, y_donors, weights)
}
\arguments{
\item{y_treated}{Numeric; combined arithmetically in the body.}
\item{y_donors}{See Usage.}
\item{weights}{A matrix; passed to \code{crossprod}.}
}
\description{
A step of the plcbsc_native implementation. Called by \code{.plcbsc_in_time_placebo}, \code{morie_plcbsc}. See the file header for the source the module follows.
}
\value{
A numeric value.
}
\keyword{internal}
