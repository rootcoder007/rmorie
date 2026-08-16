% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{particle_filter_simple}
\alias{particle_filter_simple}
\title{particle_filter_simple}
\usage{
particle_filter_simple(y, n_particles, init, step, loglik, seed = 0L)
}
\arguments{
\item{y}{See Usage.}
\item{n_particles}{See Usage.}
\item{init}{See Usage.}
\item{step}{See Usage.}
\item{loglik}{See Usage.}
\item{seed}{Defaults to \code{0L}.}
}
\description{
Part of the pftrep_native implementation; see the file header for the source it follows.
}
\value{
A list with \code{loglik}, \code{min_ess}.
}
\keyword{internal}
