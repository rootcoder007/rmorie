% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_ess_from_chains}
\alias{.morie_ess_from_chains}
\title{.morie_ess_from_chains}
\usage{
.morie_ess_from_chains(chains)
}
\arguments{
\item{chains}{A count; the body uses it as \code{matrix(...)}.}
}
\description{
A step of the mcmc_native implementation. Called by \code{morie_effective_sample_size_bayes}, \code{morie_effective_sample_size_bulk}, \code{morie_effective_sample_size_tail}. See the file header for the source the module follows.
}
\value{
One of two values, depending on the branch taken.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.morie_ess_from_chains(V)
}
