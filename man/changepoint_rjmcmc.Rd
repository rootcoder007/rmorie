% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{changepoint_rjmcmc}
\alias{changepoint_rjmcmc}
\title{changepoint_rjmcmc}
\usage{
changepoint_rjmcmc(y = numeric(0), L = 1, n_iter = 40000, burn_in = 4000, lam = 3, 
    k_max = 30, alpha = 1, beta = 200, seed = 0, cap = 0.9, use_likelihood = TRUE, 
    k_init = 0, thin = 1, keep_chain = FALSE) 
}
\arguments{
\item{y}{Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{numeric(0)}.}
\item{L}{Numeric; passed to \code{log}. Defaults to \code{1}.}
\item{n_iter}{A count; the body uses it as \code{seq_len(...)}. Defaults to \code{40000L}.}
\item{burn_in}{Numeric; combined arithmetically in the body. Defaults to \code{4000L}.}
\item{lam}{Passed to \code{.log_k_prior_ratio}. Defaults to \code{3}.}
\item{k_max}{Numeric; combined arithmetically in the body. Defaults to \code{30L}.}
\item{alpha}{Numeric; combined arithmetically in the body. Defaults to \code{1}.}
\item{beta}{Numeric; passed to \code{log}. Defaults to \code{200}.}
\item{seed}{Passed to \code{.unif_stream}. Defaults to \code{0L}.}
\item{cap}{Passed to \code{changepoint_move_probabilities}. Defaults to \code{0.9}.}
\item{use_likelihood}{A flag; the body branches on it. Defaults to \code{TRUE}.}
\item{k_init}{A count; the body uses it as \code{seq_len(...)}. Defaults to \code{0L}.}
\item{thin}{Numeric; combined arithmetically in the body. Defaults to \code{1L}.}
\item{keep_chain}{A flag; the body branches on it. Defaults to \code{FALSE}.}
}
\description{
A step of the bayrjmcmc_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{k_posterior}, \code{k_counts}, \code{k_mean}, \code{s}, \code{h}, \code{accept}, \code{tried}, \code{c}, \code{b}, \code{d}, \code{eta}, \code{pi}, \code{mean_s1_given_k1}, \code{var_s1_given_k1}, \code{mean_height}, \code{chain}, \code{n_kept}, \code{use_likelihood}, \code{method}, \code{note}.
}
\keyword{internal}
\examples{
u <- .unif_stream(1L)
unique(c(u(), u(), u(), u()))
set.seed(2); y <- sort(c(runif(30, 0, .4), runif(60, .4, 1)))
r <- changepoint_rjmcmc(y = y, L = 1, n_iter = 4000L, burn_in = 1000L, seed = 1L)
r$k_mean
r$k_posterior[1]
r2 <- changepoint_rjmcmc(y = y, L = 1, n_iter = 4000L, burn_in = 1000L, seed = 1L)
r$k_posterior
}
