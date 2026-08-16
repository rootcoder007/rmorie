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
\item{a}{See Usage.}
\item{b}{See Usage.}
}
\description{
Part of the helpers_ghc_rng implementation; see the file header for the source it follows.
}
\value{
A list with \code{hi}, \code{lo}.
}
\keyword{internal}
