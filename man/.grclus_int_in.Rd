% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.grclus_int_in}
\alias{.grclus_int_in}
\title{RNG helpers (shared) ----}
\usage{
.grclus_int_in(e, lo, hi)
}
\arguments{
\item{e}{Passed to \code{.ghc_unif}.}
\item{lo}{Numeric; combined arithmetically in the body.}
\item{hi}{Numeric; combined arithmetically in the body.}
}
\description{
A step of the grclus_native implementation. Called by \code{.grclus_grow_partition}, \code{.grclus_match_vertices}, \code{.grclus_shuffled}. See the file header for the source the module follows.
}
\value{
A numeric value.
}
\keyword{internal}
