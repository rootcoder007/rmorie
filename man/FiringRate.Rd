% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{FiringRate}
\alias{FiringRate}
\title{MFR = 1/mean(IDI), CV = SD(IDI)/mean(IDI).  MFR is the RECIPROCAL OF}
\usage{
FiringRate(times, fs = NULL)
}
\arguments{
\item{times}{See Usage.}
\item{fs}{Defaults to \code{NULL}.}
}
\description{
THE MEAN interval, not the mean of the reciprocals; the two differ whenever the intervals vary, and only the former equals discharges per unit time.  Both are returned.
}
\value{
A list with \code{mfr}, \code{mean_idi}, \code{sd_idi}, \code{cv_idi}, \code{idi}, \code{n_discharges}, \code{mean_instantaneous_rate}, \code{duration}, \code{method}.
}
\keyword{internal}
