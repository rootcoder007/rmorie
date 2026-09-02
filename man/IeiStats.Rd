% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{IeiStats}
\alias{IeiStats}
\title{IeiStats: point-process inter-event interval statistics}
\usage{
IeiStats(event_times, T = NULL, n_bins = 20) 
}
\arguments{
\item{event_times}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{T}{Optional; may be \code{NULL}. Coerced to numeric by the body, with \code{as.numeric}.}
\item{n_bins}{Numeric; combined arithmetically in the body. Defaults to \code{20}.}
}
\description{
A step of the rangayyan_phys implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{mean_ipi_s}, \code{sd_ipi_s}, \code{cv_ipi}, \code{mean_rate_pps}, \code{sd_rate_pps}, \code{cv_rate}, \code{event_rate_pps}, \code{min_ipi_s}, \code{max_ipi_s}, \code{median_ipi_s}, \code{fano_factor}, \code{ipi_histogram}, \code{n_events}, \code{n_intervals}, \code{duration_s}, \code{regularity}, \code{units}, \code{method}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
IeiStats(V)
}
