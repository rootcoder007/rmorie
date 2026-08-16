% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.dpgrf_adjacency_pairs}
\alias{.dpgrf_adjacency_pairs}
\title{Adjacent pairs of regions}
\usage{
.dpgrf_adjacency_pairs(W)
}
\arguments{
\item{W}{Square symmetric adjacency matrix (n x n).}
}
\description{
Enumerates the unordered pairs of regions with a nonzero entry in the (symmetric) adjacency matrix. Only \code{i < j} pairs are returned, so each boundary sits on exactly one pair.
}
\value{
A list with \code{pairs} (list of \code{c(i, j)} with   \code{i < j}), \code{n_pairs}, \code{n_regions}, \code{degrees}   (row sums).
}
\keyword{internal}
