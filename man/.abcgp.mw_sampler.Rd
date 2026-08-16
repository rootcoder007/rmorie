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
\item{sim}{Accepted by the signature and not used anywhere in the body.}
\item{obs}{Passed to \code{.abcgp.synthetic_log_likelihood}.}
\item{log_prior}{Passed to \code{.abcgp.alpha_terms}.}
\item{theta0}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{n_iter}{Numeric; combined arithmetically in the body.}
\item{n_sim}{Coerced to integer by the body, with \code{as.integer}.}
\item{epsilon}{Passed to \code{.abcgp.synthetic_log_likelihood}.}
\item{proposal_sd}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{summary}{Passed to \code{.abcgp.synthetic_log_likelihood}.}
\item{seed}{Passed to \code{.ghc_rng}.}
\item{adaptive}{A flag; the body branches on it.}
\item{xi}{Passed to \code{<}.}
\item{delta_s}{Coerced to integer by the body, with \code{as.integer}.}
\item{n_alpha}{Coerced to integer by the body, with \code{as.integer}.}
\item{max_sim}{Optional; may be \code{NULL}. Coerced to integer by the body, with \code{as.integer}.}
}
\description{
A step of the abcgp_native implementation. Called by \code{.abcgp.gps_abc}, \code{.abcgp.synthetic_abc}. See the file header for the source the module follows.
}
\value{
A list with \code{chain}, \code{acceptance_rate}, \code{n_simulations}, \code{unresolved_steps}.
}
\keyword{internal}
