% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.trend_vector}
\alias{.trend_vector}
\title{.trend_vector}
\usage{
.trend_vector(torsion_sets, activities, permutations, seed)
}
\arguments{
\item{torsion_sets}{Iterated over elementwise, with \code{lapply}.}
\item{activities}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{permutations}{A count; the body uses it as \code{seq_len(...)}.}
\item{seed}{Passed to \code{.ghc_rng}.}
}
\description{
A step of the toptor_native implementation. Called by \code{morie_toptor}. See the file header for the source the module follows.
}
\value{
A list with \code{vector}, \code{descriptors}, \code{length}, \code{null_mean}, \code{null_sd}, \code{z}.
}
\keyword{internal}
