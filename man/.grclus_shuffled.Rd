% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.grclus_shuffled}
\alias{.grclus_shuffled}
\title{.grclus_shuffled}
\usage{
.grclus_shuffled(n, seed)
}
\arguments{
\item{n}{A count; the body uses it as \code{seq_len(...)}.}
\item{seed}{Passed to \code{.ghc_rng}.}
}
\description{
A step of the grclus_native implementation. Called by \code{.grclus_match_vertices}. See the file header for the source the module follows.
}
\value{
The value of \code{idx}, as built in the body.
}
\keyword{internal}
