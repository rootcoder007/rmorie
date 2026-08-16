% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_prtcl_particle_filter}
\alias{morie_prtcl_particle_filter}
\title{morie_prtcl_particle_filter}
\usage{
morie_prtcl_particle_filter(y, n.particles, init, step, loglik, seed = 0L, resample.threshold = 1, systematic = TRUE)
}
\arguments{
\item{y}{See Usage.}
\item{n.particles}{See Usage.}
\item{init}{See Usage.}
\item{step}{See Usage.}
\item{loglik}{See Usage.}
\item{seed}{Defaults to \code{0L}.}
\item{resample.threshold}{Defaults to \code{1}.}
\item{systematic}{Defaults to \code{TRUE}.}
}
\description{
Part of the prtcl_native implementation; see the file header for the source it follows.
}
\value{
A list with \code{estimate}, \code{filtered.mean}, \code{loglik}, \code{ess}, \code{min.ess}, \code{resampled}, \code{n.particles}, \code{n.obs}, \code{systematic}, \code{particles}, \code{method}.
}
\keyword{internal}
