% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_geron_weight_pruning_hm}
\alias{morie_geron_weight_pruning_hm}
\title{Global magnitude weight pruning to an exact sparsity, cubic ramp schedule (Geron Ch 17, hmpru)}
\usage{
morie_geron_weight_pruning_hm(model, sparsity, n_rounds = 1)
}
\arguments{
\item{model}{Numeric vector or named list.}
\item{sparsity}{Target fraction of zeros in \[0, 1).}
\item{n_rounds}{Rounds in the returned schedule.}
}
\description{
Global magnitude weight pruning to an exact sparsity, cubic ramp schedule (Geron Ch 17, hmpru)
}
\value{
A list with \code{pruned}, \code{mask}, \code{threshold}, \code{achieved_sparsity}, \code{n_pruned}, \code{n_weights}, \code{schedule}, \code{estimate}, \code{n}, \code{method}.
}
\keyword{internal}
