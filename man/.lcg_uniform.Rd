% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.lcg_uniform}
\alias{.lcg_uniform}
\title{The Python arm draws from np.random.default_rng, i.e. SplitMix64 --}
\usage{
.lcg_uniform(seed, n)
}
\arguments{
\item{seed}{See Usage.}
\item{n}{See Usage.}
}
\description{
not an LCG at all. Making the LCG exact would still have left the two arms on different generators, so this takes the bit-identical mirror of default_rng that .ghc_rng already provides.
}
\value{
The value of \code{.ghc_unif}.
}
\keyword{internal}
