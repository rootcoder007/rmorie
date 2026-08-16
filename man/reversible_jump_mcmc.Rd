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
\item{models}{A vector; its length is taken and its elements indexed.}
\item{moves}{See Usage.}
\item{init_model}{See Usage.}
\item{init_theta}{Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{c()}.}
\item{n_iter}{A count; the body uses it as \code{seq_len(...)}. Defaults to \code{10000L}.}
\item{burn_in}{Numeric; combined arithmetically in the body. Defaults to \code{0L}.}
\item{thin}{Numeric; combined arithmetically in the body. Defaults to \code{1L}.}
\item{seed}{Passed to \code{.unif_stream}. Defaults to \code{0L}.}
\item{within}{Optional; may be \code{NULL}. A vector; indexed elementwise.}
\item{within_scale}{Passed to \code{.rw_within}. Defaults to \code{0.5}.}
\item{within_weight}{Carried through into a list the body builds. Defaults to \code{1}.}
\item{move_weight}{Defaults to \code{1}.}
\item{jacobian}{One of \code{"analytic"}, \code{"numeric"}. Defaults to \code{"analytic"}.}
\item{keep_chain}{A flag; the body branches on it. Defaults to \code{TRUE}.}
}
\description{
A step of the bayrjmcmc_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{model_freq}, \code{visits}, \code{n_kept}, \code{accept}, \code{tried}, \code{chain}, \code{jacobian}, \code{method}, \code{note}.
}
\keyword{internal}
