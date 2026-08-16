% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_rank_normalize}
\alias{.morie_rank_normalize}
\title{.morie_rank_normalize}
\usage{
.morie_rank_normalize(C)
}
\arguments{
\item{C}{A matrix; passed to \code{nrow}.}
}
\description{
A step of the mcmc_native implementation. Called by \code{.morie_split_rhat}, \code{morie_effective_sample_size_bulk}. See the file header for the source the module follows.
}
\value{
A matrix, from \code{matrix}.
}
\keyword{internal}
