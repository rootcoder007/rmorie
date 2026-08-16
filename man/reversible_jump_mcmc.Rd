% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{reversible_jump_mcmc}
\alias{reversible_jump_mcmc}
\title{reversible_jump_mcmc}
\usage{
reversible_jump_mcmc(models, moves, init_model, init_theta = c(), n_iter = 10000L, burn_in = 0L, thin = 1L, seed = 0L, within = NULL, within_scale = 0.5, within_weight = 1, move_weight = 1, jacobian = "analytic", keep_chain = TRUE)
}
\arguments{
\item{models}{See Usage.}
\item{moves}{See Usage.}
\item{init_model}{See Usage.}
\item{init_theta}{Defaults to \code{c()}.}
\item{n_iter}{Defaults to \code{10000L}.}
\item{burn_in}{Defaults to \code{0L}.}
\item{thin}{Defaults to \code{1L}.}
\item{seed}{Defaults to \code{0L}.}
\item{within}{Defaults to \code{NULL}.}
\item{within_scale}{Defaults to \code{0.5}.}
\item{within_weight}{Defaults to \code{1}.}
\item{move_weight}{Defaults to \code{1}.}
\item{jacobian}{Defaults to \code{"analytic"}.}
\item{keep_chain}{Defaults to \code{TRUE}.}
}
\description{
Part of the bayrjmcmc_native implementation; see the file header for the source it follows.
}
\value{
A list with \code{model_freq}, \code{visits}, \code{n_kept}, \code{accept}, \code{tried}, \code{chain}, \code{jacobian}, \code{method}, \code{note}.
}
\keyword{internal}
