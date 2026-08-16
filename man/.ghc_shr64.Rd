% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.ghc_shr64}
\alias{.ghc_shr64}
\title{Logical right shift, 0 < k < 32 (the only widths SplitMix64 uses)}
\usage{
.ghc_shr64(a, k)
}
\arguments{
\item{a}{A list; the body reads \code{$hi}, \code{$lo} from it.}
\item{k}{Numeric; combined arithmetically in the body.}
}
\description{
A step of the helpers_ghc_rng implementation. Called by \code{.ghc_int}, \code{.ghc_unif}. See the file header for the source the module follows.
}
\value{
A list with \code{hi}, \code{lo}.
}
\keyword{internal}
