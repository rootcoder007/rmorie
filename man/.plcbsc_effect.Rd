% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.plcbsc_effect}
\alias{.plcbsc_effect}
\title{.plcbsc_effect}
\usage{
.plcbsc_effect(gaps, t0, statistic, pre_gaps = NULL)
}
\arguments{
\item{gaps}{A vector; its length is taken and its elements indexed.}
\item{t0}{A count; the body uses it as \code{seq_len(...)}.}
\item{statistic}{Compared against \code{"effect"}.}
\item{pre_gaps}{Defaults to \code{NULL}.}
}
\description{
A step of the plcbsc_native implementation. Called by \code{morie_plcbsc}. See the file header for the source the module follows.
}
\value{
A numeric value.
}
\keyword{internal}
