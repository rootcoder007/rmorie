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
\item{alignment}{See Usage.}
\item{n_iter}{Defaults to \code{2000}.}
\item{burnin}{Defaults to \code{NULL}.}
\item{n_chains}{Defaults to \code{4}.}
\item{lam}{Defaults to \code{0.2}.}
\item{swap_every}{Defaults to \code{10}.}
\item{sample_every}{Defaults to \code{10}.}
\item{pi}{Defaults to \code{NULL}.}
\item{rate}{Defaults to \code{1}.}
\item{branch_prior_mean}{Defaults to \code{0.1}.}
\item{partitions}{Defaults to \code{NULL}.}
\item{tree}{Defaults to \code{NULL}.}
\item{n_runs}{Defaults to \code{2}.}
\item{tune}{Defaults to \code{1}.}
\item{seed}{Defaults to \code{0}.}
}
\description{
Part of the phylby_native implementation; see the file header for the source it follows.
}
\value{
The value of \code{result}, as built in the body.
}
\keyword{internal}
