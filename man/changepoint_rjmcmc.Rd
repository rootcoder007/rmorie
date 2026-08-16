% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{changepoint_rjmcmc}
\alias{changepoint_rjmcmc}
\title{changepoint_rjmcmc}
\usage{
changepoint_rjmcmc(y = numeric(0), L = 1, n_iter = 40000L, burn_in = 4000L, lam = 3, k_max = 30L, alpha = 1, beta = 200, seed = 0L, cap = 0.9, use_likelihood = TRUE, k_init = 0L, thin = 1L, keep_chain = FALSE)
}
\arguments{
\item{y}{Defaults to \code{numeric(0)}.}
\item{L}{Defaults to \code{1}.}
\item{n_iter}{Defaults to \code{40000L}.}
\item{burn_in}{Defaults to \code{4000L}.}
\item{lam}{Defaults to \code{3}.}
\item{k_max}{Defaults to \code{30L}.}
\item{alpha}{Defaults to \code{1}.}
\item{beta}{Defaults to \code{200}.}
\item{seed}{Defaults to \code{0L}.}
\item{cap}{Defaults to \code{0.9}.}
\item{use_likelihood}{Defaults to \code{TRUE}.}
\item{k_init}{Defaults to \code{0L}.}
\item{thin}{Defaults to \code{1L}.}
\item{keep_chain}{Defaults to \code{FALSE}.}
}
\description{
Part of the bayrjmcmc_native implementation; see the file header for the source it follows.
}
\value{
A list with \code{k_posterior}, \code{k_counts}, \code{k_mean}, \code{s}, \code{h}, \code{accept}, \code{tried}, \code{c}, \code{b}, \code{d}, \code{eta}, \code{pi}, \code{mean_s1_given_k1}, \code{var_s1_given_k1}, \code{mean_height}, \code{chain}, \code{n_kept}, \code{use_likelihood}, \code{method}, \code{note}.
}
\keyword{internal}
