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
\item{n_particles}{A count; the body uses it as \code{rep(...)}.}
\item{init}{See Usage.}
\item{step}{See Usage.}
\item{loglik}{See Usage.}
\item{seed}{Coerced to integer by the body, with \code{as.integer}. Defaults to \code{0L}.}
}
\description{
A step of the pftrep_native implementation. Called by \code{replicated_pfilter}. See the file header for the source the module follows.
}
\value{
A list with \code{loglik}, \code{min_ess}.
}
\keyword{internal}
