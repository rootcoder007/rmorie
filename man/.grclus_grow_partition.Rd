% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.grclus_grow_partition}
\alias{.grclus_grow_partition}
\title{.grclus_grow_partition}
\usage{
.grclus_grow_partition(adj, vw, target, seed, greedy, n_starts = NULL)
}
\arguments{
\item{adj}{A vector; its length is taken and its elements indexed.}
\item{vw}{A vector; indexed elementwise.}
\item{target}{Passed to \code{<}.}
\item{seed}{Passed to \code{.ghc_rng}.}
\item{greedy}{A flag; the body branches on it.}
\item{n_starts}{Optional; may be \code{NULL}. A count; the body uses it as \code{seq_len(...)}.}
}
\description{
A step of the grclus_native implementation. Called by \code{.grclus_bisect}. See the file header for the source the module follows.
}
\value{
The value of \code{best_parts}, as built in the body.
}
\keyword{internal}
