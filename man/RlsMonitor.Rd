% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{RlsMonitor}
\alias{RlsMonitor}
\title{RlsMonitor}
\usage{
RlsMonitor(x, reference = NULL, order = 8, lam = 0.98, settle = NULL, threshold = 3, window = NULL)
}
\arguments{
\item{x}{See Usage.}
\item{reference}{Defaults to \code{NULL}.}
\item{order}{Defaults to \code{8}.}
\item{lam}{Defaults to \code{0.98}.}
\item{settle}{Defaults to \code{NULL}.}
\item{threshold}{Defaults to \code{3}.}
\item{window}{Defaults to \code{NULL}.}
}
\description{
A step of the rangayyan_adapt implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{error}, \code{error_power}, \code{boundaries}, \code{n_boundaries}, \code{threshold}, \code{baseline}, \code{baseline_sd}, \code{settle}, \code{window}, \code{order}, \code{transient_excluded}, \code{method}.
}
\keyword{internal}
