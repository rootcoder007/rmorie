% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_wsm_boot_reps}
\alias{.morie_wsm_boot_reps}
\title{.morie_wsm_boot_reps}
\usage{
.morie_wsm_boot_reps(data, T, B, seed)
}
\arguments{
\item{data}{A vector; its length is taken and its elements indexed.}
\item{T}{See Usage.}
\item{B}{A count; the body uses it as \code{seq_len(...)}.}
\item{seed}{Passed to \code{.morie_wsm_lcg_u}.}
}
\description{
A step of the wasserman_native implementation. Called by \code{morie_wasserman_bootstrap_percentile}, \code{morie_wasserman_bootstrap_pivotal}, \code{morie_wasserman_nonparametric_boot}. See the file header for the source the module follows.
}
\value{
A vector, from \code{vapply}.
}
\keyword{internal}
