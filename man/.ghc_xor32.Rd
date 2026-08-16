% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.ghc_xor32}
\alias{.ghc_xor32}
\title{BitwXor is signed-32-bit; split into 16-bit halves to stay in range}
\usage{
.ghc_xor32(a, b)
}
\arguments{
\item{a}{Numeric; combined arithmetically in the body.}
\item{b}{Numeric; combined arithmetically in the body.}
}
\description{
A step of the helpers_ghc_rng implementation. Called by \code{.ghc_xor64}. See the file header for the source the module follows.
}
\value{
A numeric value.
}
\keyword{internal}
