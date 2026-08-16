% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_phylby}
\alias{morie_phylby}
\title{morie_phylby}
\usage{
morie_phylby(alignment, n_iter = 2000, burnin = NULL, n_chains = 4, lam = 0.2, swap_every = 10, sample_every = 10, pi = NULL, rate = 1, branch_prior_mean = 0.1, partitions = NULL, tree = NULL, n_runs = 2, tune = 1, seed = 0)
}
\arguments{
\item{alignment}{A vector; indexed elementwise.}
\item{n_iter}{Numeric; combined arithmetically in the body. Defaults to \code{2000}.}
\item{burnin}{Optional; may be \code{NULL}. Coerced to integer by the body, with \code{as.integer}.}
\item{n_chains}{Numeric; combined arithmetically in the body. Defaults to \code{4}.}
\item{lam}{Passed to \code{morie_phylby_chain_temperature}. Defaults to \code{0.2}.}
\item{swap_every}{Coerced to integer by the body, with \code{as.integer}. Defaults to \code{10}.}
\item{sample_every}{Coerced to integer by the body, with \code{as.integer}. Defaults to \code{10}.}
\item{pi}{Passed to \code{.phylby_step}.}
\item{rate}{Defaults to \code{1}.}
\item{branch_prior_mean}{Passed to \code{.phylby_step}. Defaults to \code{0.1}.}
\item{partitions}{Optional; may be \code{NULL}. A vector; its length is taken.}
\item{tree}{Defaults to \code{NULL}.}
\item{n_runs}{Numeric; combined arithmetically in the body. Defaults to \code{2}.}
\item{tune}{Passed to \code{.phylby_step}. Defaults to \code{1}.}
\item{seed}{Numeric; combined arithmetically in the body. Defaults to \code{0}.}
}
\description{
A step of the phylby_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
The value of \code{result}, as built in the body.
}
\keyword{internal}
