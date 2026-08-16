% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{fit_seed_state}
\alias{fit_seed_state}
\title{fit_seed_state}
\usage{
fit_seed_state(z, spec, theta, long_run_b = 0)
}
\arguments{
\item{z}{A vector; its length is taken.}
\item{spec}{Passed to \code{n_states}.}
\item{theta}{Passed to \code{bats_filter}.}
\item{long_run_b}{Passed to \code{bats_filter}. Defaults to \code{0}.}
}
\description{
A step of the bats_native implementation. Called by \code{.fit_spec}. See the file header for the source the module follows.
}
\value{
A vector, from \code{as.numeric}.
}
\keyword{internal}
