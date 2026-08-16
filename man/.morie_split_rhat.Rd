% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_split_rhat}
\alias{.morie_split_rhat}
\title{.morie_split_rhat}
\usage{
.morie_split_rhat(chains, rank_normalized = TRUE)
}
\arguments{
\item{chains}{A count; the body uses it as \code{matrix(...)}.}
\item{rank_normalized}{A flag; the body branches on it. Defaults to \code{TRUE}.}
}
\description{
A step of the mcmc_native implementation. Called by \code{morie_effective_sample_size_bayes}. See the file header for the source the module follows.
}
\value{
A numeric value.
}
\keyword{internal}
