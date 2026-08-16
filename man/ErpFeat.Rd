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
\item{fs}{Numeric; combined arithmetically in the body.}
\item{t0}{Numeric; combined arithmetically in the body. Defaults to \code{0}.}
\item{components}{Optional; may be \code{NULL}. A vector; indexed elementwise.}
\item{baseline_ms}{A vector; indexed elementwise. Defaults to \code{list(NULL, 0)}.}
}
\description{
A step of the rangayyan_phys implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{components}, \code{peak_to_peak_uV}, \code{baseline_uV}, \code{t_ms}, \code{erp_uV}, \code{fs_hz}, \code{t0_ms}, \code{units}, \code{method}.
}
\keyword{internal}
\examples{
ErpFeat(erp = c(1, 2, 3, 4, 5, 6, 7, 8), fs = 5L)
}
