% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{replicated_pfilter}
\alias{replicated_pfilter}
\title{replicated_pfilter}
\usage{
replicated_pfilter(y, n_particles, init, step, loglik, n_reps = 10L, seed = 0L)
}
\arguments{
\item{y}{See Usage.}
\item{n_particles}{See Usage.}
\item{init}{See Usage.}
\item{step}{See Usage.}
\item{loglik}{See Usage.}
\item{n_reps}{Defaults to \code{10L}.}
\item{seed}{Numeric; combined arithmetically in the body. Defaults to \code{0L}.}
}
\description{
A step of the pftrep_native implementation. Called by \code{loglik_profile}. See the file header for the source the module follows.
}
\value{
A list with \code{estimate}, \code{loglik}, \code{logmeanexp}, \code{mean_loglik}, \code{jensen_gap}, \code{se}, \code{replicates}, \code{n_reps}, \code{n_particles}, \code{min_ess}, \code{mean_min_ess}, \code{method}.
}
\keyword{internal}
