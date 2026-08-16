% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.ghc_mul64}
\alias{.ghc_mul64}
\title{(a * b) mod 2^64: only the low word of each cross term survives}
\usage{
.ghc_mul64(a, b)
}
\arguments{
\item{a}{A list; the body reads \code{$hi}, \code{$lo} from it.}
\item{b}{A vector; indexed elementwise.}
}
\description{
A step of the helpers_ghc_rng implementation. Called by \code{.ghc_int}, \code{.ghc_unif}, \code{.survrsf_rng}. See the file header for the source the module follows.
}
\value{
A list with \code{hi}, \code{lo}.
}
\keyword{internal}
