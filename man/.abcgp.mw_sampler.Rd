% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.abcgp.mw_sampler}
\alias{.abcgp.mw_sampler}
\title{.abcgp.mw_sampler}
\usage{
.abcgp.mw_sampler(sim, obs, log_prior, theta0, n_iter, n_sim, epsilon, proposal_sd, summary, seed, adaptive, xi, delta_s, n_alpha, max_sim = NULL)
}
\arguments{
\item{sim}{See Usage.}
\item{obs}{See Usage.}
\item{log_prior}{See Usage.}
\item{theta0}{See Usage.}
\item{n_iter}{See Usage.}
\item{n_sim}{See Usage.}
\item{epsilon}{See Usage.}
\item{proposal_sd}{See Usage.}
\item{summary}{See Usage.}
\item{seed}{See Usage.}
\item{adaptive}{See Usage.}
\item{xi}{See Usage.}
\item{delta_s}{See Usage.}
\item{n_alpha}{See Usage.}
\item{max_sim}{Defaults to \code{NULL}.}
}
\description{
Part of the abcgp_native implementation; see the file header for the source it follows.
}
\value{
A list with \code{chain}, \code{acceptance_rate}, \code{n_simulations}, \code{unresolved_steps}.
}
\keyword{internal}
