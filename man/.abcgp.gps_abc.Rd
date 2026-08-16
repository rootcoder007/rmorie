% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.abcgp.gps_abc}
\alias{.abcgp.gps_abc}
\title{.abcgp.gps_abc}
\usage{
.abcgp.gps_abc(sim, obs, log_prior, theta0, n_iter = 200, n_sim = 10, epsilon = 0, proposal_sd = 0.5, summary = NULL, seed = 0, xi = 0.05, delta_s = 10, n_alpha = 64, max_sim = 400)
}
\arguments{
\item{sim}{Passed to \code{.abcgp.mw_sampler}.}
\item{obs}{Passed to \code{.abcgp.mw_sampler}.}
\item{log_prior}{Passed to \code{.abcgp.mw_sampler}.}
\item{theta0}{Passed to \code{.abcgp.mw_sampler}.}
\item{n_iter}{Passed to \code{.abcgp.mw_sampler}. Defaults to \code{200}.}
\item{n_sim}{Passed to \code{.abcgp.mw_sampler}. Defaults to \code{10}.}
\item{epsilon}{Passed to \code{.abcgp.mw_sampler}. Defaults to \code{0}.}
\item{proposal_sd}{Passed to \code{.abcgp.mw_sampler}. Defaults to \code{0.5}.}
\item{summary}{Passed to \code{.abcgp.mw_sampler}.}
\item{seed}{Passed to \code{.abcgp.mw_sampler}. Defaults to \code{0}.}
\item{xi}{Passed to \code{.abcgp.mw_sampler}. Defaults to \code{0.05}.}
\item{delta_s}{Passed to \code{.abcgp.mw_sampler}. Defaults to \code{10}.}
\item{n_alpha}{Passed to \code{.abcgp.mw_sampler}. Defaults to \code{64}.}
\item{max_sim}{Passed to \code{.abcgp.mw_sampler}. Defaults to \code{400}.}
}
\description{
A step of the abcgp_native implementation. Called by \code{morie_abcgp}. See the file header for the source the module follows.
}
\value{
The value of \code{.abcgp.mw_sampler}.
}
\keyword{internal}
