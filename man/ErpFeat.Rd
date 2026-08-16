% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{ErpFeat}
\alias{ErpFeat}
\title{ErpFeat: event-related potential latency and amplitude features}
\usage{
ErpFeat(erp, fs, t0 = 0, components = NULL, baseline_ms = list(NULL, 0))
}
\arguments{
\item{erp}{See Usage.}
\item{fs}{See Usage.}
\item{t0}{Defaults to \code{0}.}
\item{components}{Defaults to \code{NULL}.}
\item{baseline_ms}{Defaults to \code{list(NULL, 0)}.}
}
\description{
Part of the rangayyan_phys implementation; see the file header for the source it follows.
}
\value{
A list with \code{components}, \code{peak_to_peak_uV}, \code{baseline_uV}, \code{t_ms}, \code{erp_uV}, \code{fs_hz}, \code{t0_ms}, \code{units}, \code{method}.
}
\keyword{internal}
