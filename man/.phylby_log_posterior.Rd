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
\item{tree}{See Usage.}
\item{seqs}{See Usage.}
\item{pi}{Defaults to \code{NULL}.}
\item{rate}{Defaults to \code{1}.}
\item{branch_prior_mean}{Defaults to \code{0.1}.}
\item{partitions}{Defaults to \code{NULL}.}
\item{rates}{Defaults to \code{NULL}.}
\item{temperature}{Defaults to \code{1}.}
}
\description{
Part of the phylby_native implementation; see the file header for the source it follows.
}
\value{
A list with \code{loglik}, \code{logprior}, \code{logpost}.
}
\keyword{internal}
