% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{sample_neighbors}
\alias{sample_neighbors}
\title{Sample a fixed number of neighbours with replacement}
\usage{
sample_neighbors(adj, v, size, rng = NULL)
}
\arguments{
\item{adj}{Adjacency list keyed by 0-based node id (character keys).}
\item{v}{Node to sample around.}
\item{size}{Number of neighbours to draw.}
\item{rng}{Optional generator environment (defaults to .ghc_rng(0)).}
}
\description{
Sample a fixed number of neighbours with replacement
}
\value{
Vector of sampled neighbour ids.
}
\keyword{internal}
\examples{
rng <- .ghc_rng(0L)
adj <- list(`0` = c(`1` = 1, `3` = 1), `1` = c(`0` = 1, `2` = 1),
            `2` = c(`1` = 1, `3` = 1), `3` = c(`2` = 1, `0` = 1))
nb <- sample_neighbors(adj, v = 0, size = 2, rng)
length(nb) == 2L
}
