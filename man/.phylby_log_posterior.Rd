% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.phylby_log_posterior}
\alias{.phylby_log_posterior}
\title{.phylby_log_posterior}
\usage{
.phylby_log_posterior(tree, seqs, pi = NULL, rate = 1, branch_prior_mean = 0.1, partitions = NULL, rates = NULL, temperature = 1)
}
\arguments{
\item{tree}{Passed to \code{.phylby_branch_paths}.}
\item{seqs}{A vector; indexed elementwise.}
\item{pi}{Passed to \code{morie_phylml}.}
\item{rate}{Numeric; combined arithmetically in the body. Defaults to \code{1}.}
\item{branch_prior_mean}{Numeric; combined arithmetically in the body. Defaults to \code{0.1}.}
\item{partitions}{Defaults to \code{NULL}.}
\item{rates}{Optional; may be \code{NULL}. A vector; indexed elementwise.}
\item{temperature}{Numeric; combined arithmetically in the body. Defaults to \code{1}.}
}
\description{
A step of the phylby_native implementation. Called by \code{.phylby_step}, \code{morie_phylby_log_posterior}. See the file header for the source the module follows.
}
\value{
A list with \code{loglik}, \code{logprior}, \code{logpost}.
}
\keyword{internal}
