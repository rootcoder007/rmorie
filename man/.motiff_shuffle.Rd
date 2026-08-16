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
\item{adj}{See Usage.}
\item{n}{See Usage.}
\item{e}{See Usage.}
\item{swaps}{See Usage.}
\item{preserve_mutual}{See Usage.}
}
\description{
row-major classify into single edges and mutual (bidirectional) pairs; single<->single and mutual<->mutual switches only, so the mutual-edge count is invariant; identical RNG draw order.
}
\value{
The value of \code{new}, as built in the body.
}
\keyword{internal}
