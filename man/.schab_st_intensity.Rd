% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.schab_st_intensity}
\alias{.schab_st_intensity}
\title{.schab_st_intensity}
\usage{
.schab_st_intensity(points, times, region, time_interval)
}
\arguments{
\item{points}{A matrix; passed to \code{as.matrix}.}
\item{times}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{region}{Passed to \code{.schab_st_region_box}.}
\item{time_interval}{A vector; indexed elementwise.}
}
\description{
A step of the schab_st_shared implementation. Called by \code{spstp}. See the file header for the source the module follows.
}
\value{
A list with \code{intensity}, \code{n}, \code{area}, \code{duration}, \code{volume}.
}
\keyword{internal}
