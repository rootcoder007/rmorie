% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_confidence_set}
\alias{morie_confidence_set}
\title{morie_confidence_set}
\usage{
morie_confidence_set(moment_fn, theta_grid, X, form = "sum", n_equality = 0L, level = 0.95, n_levels = 2L, reps = 100L, seed = 0)
}
\arguments{
\item{moment_fn}{See Usage.}
\item{theta_grid}{See Usage.}
\item{X}{Passed to \code{morie_hypercube_instruments}.}
\item{form}{Passed to \code{morie_cvm_statistic}. Defaults to \code{"sum"}.}
\item{n_equality}{Passed to \code{morie_cvm_statistic}. Defaults to \code{0L}.}
\item{level}{Passed to \code{morie_gms_critical_value}. Defaults to \code{0.95}.}
\item{n_levels}{Passed to \code{morie_hypercube_instruments}. Defaults to \code{2L}.}
\item{reps}{Passed to \code{morie_gms_critical_value}. Defaults to \code{100L}.}
\item{seed}{Passed to \code{morie_gms_critical_value}. Defaults to \code{0}.}
}
\description{
A step of the bndsmw_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{estimate}, \code{set}, \code{n_in_set}, \code{bounds}, \code{statistics}, \code{form}, \code{level}, \code{n_instruments}, \code{method}.
}
\keyword{internal}
