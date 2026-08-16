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
\item{y_treated}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{y_donors}{Iterated over elementwise, with \code{lapply}.}
\item{t0}{Numeric; combined arithmetically in the body.}
\item{fake_t0}{A count; the body uses it as \code{seq_len(...)}.}
\item{v}{Carried through into a list the body builds.}
\item{...}{Passed through.}
}
\description{
A step of the plcbsc_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{weights}, \code{gaps}, \code{placebo_effect}, \code{rmspe_pre}, \code{rmspe_placebo}.
}
\keyword{internal}
