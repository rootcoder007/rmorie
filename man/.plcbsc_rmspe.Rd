% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.plcbsc_rmspe}
\alias{.plcbsc_rmspe}
\title{.plcbsc_rmspe}
\usage{
.plcbsc_rmspe(gaps)
}
\arguments{
\item{gaps}{A vector; its length is taken.}
}
\description{
A step of the plcbsc_native implementation. Called by \code{.plcbsc_effect}, \code{.plcbsc_in_time_placebo}, \code{morie_plcbsc}. See the file header for the source the module follows.
}
\value{
A numeric value.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.plcbsc_rmspe(V)
}
