% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.ghc_mul32}
\alias{.ghc_mul32}
\title{Exact 32x32 -> 64 via 16-bit limbs; `a` a vector, `b` a scalar}
\usage{
.ghc_mul32(a, b)
}
\arguments{
\item{a}{Numeric; combined arithmetically in the body.}
\item{b}{Numeric; combined arithmetically in the body.}
}
\description{
A step of the helpers_ghc_rng implementation. Called by \code{.ghc_mul64}. See the file header for the source the module follows.
}
\value{
A list with \code{hi}, \code{lo}.
}
\keyword{internal}
