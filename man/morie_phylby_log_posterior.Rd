% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_phylby_log_posterior}
\alias{morie_phylby_log_posterior}
\title{morie_phylby_log_posterior}
\usage{
morie_phylby_log_posterior(tree, seqs, pi = NULL, rate = 1, branch_prior_mean = 0.1, partitions = NULL, rates = NULL, temperature = 1)
}
\arguments{
\item{tree}{Passed to \code{.phylby_log_posterior}.}
\item{seqs}{Passed to \code{.phylby_log_posterior}.}
\item{pi}{Passed to \code{.phylby_log_posterior}.}
\item{rate}{Passed to \code{.phylby_log_posterior}. Defaults to \code{1}.}
\item{branch_prior_mean}{Passed to \code{.phylby_log_posterior}. Defaults to \code{0.1}.}
\item{partitions}{Passed to \code{.phylby_log_posterior}.}
\item{rates}{Passed to \code{.phylby_log_posterior}.}
\item{temperature}{Passed to \code{.phylby_log_posterior}. Defaults to \code{1}.}
}
\description{
A step of the phylby_native implementation. Called by \code{morie_phylby}. See the file header for the source the module follows.
}
\value{
The value of \code{.phylby_log_posterior}.
}
\keyword{internal}
\examples{
morie_phylby_log_posterior(tree = list(a = 1, b = 2), seqs = c("a", "b", "c"))
}
