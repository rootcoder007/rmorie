% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.motiff_shuffle}
\alias{.motiff_shuffle}
\title{Mfinder switching (Milo refs. 17-18) mirroring Python exactly:}
\usage{
.motiff_shuffle(adj, n, e, swaps, preserve_mutual)
}
\arguments{
\item{adj}{A matrix; indexed by row and column.}
\item{n}{A count; the body uses it as \code{seq_len(...)}.}
\item{e}{Passed to \code{.ghc_unif}.}
\item{swaps}{A count; the body uses it as \code{seq_len(...)}.}
\item{preserve_mutual}{A flag; the body branches on it.}
}
\description{
row-major classify into single edges and mutual (bidirectional) pairs; single<->single and mutual<->mutual switches only, so the mutual-edge count is invariant; identical RNG draw order.
}
\value{
The value of \code{new}, as built in the body.
}
\keyword{internal}
