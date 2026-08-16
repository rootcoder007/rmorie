% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.plcbsc_in_time_placebo}
\alias{.plcbsc_in_time_placebo}
\title{.plcbsc_in_time_placebo}
\usage{
.plcbsc_in_time_placebo(y_treated, y_donors, t0, fake_t0, v = NULL, ...)
}
\arguments{
\item{y_treated}{See Usage.}
\item{y_donors}{See Usage.}
\item{t0}{See Usage.}
\item{fake_t0}{See Usage.}
\item{v}{Defaults to \code{NULL}.}
\item{...}{Passed through.}
}
\description{
Part of the plcbsc_native implementation; see the file header for the source it follows.
}
\value{
A list with \code{weights}, \code{gaps}, \code{placebo_effect}, \code{rmspe_pre}, \code{rmspe_placebo}.
}
\keyword{internal}
