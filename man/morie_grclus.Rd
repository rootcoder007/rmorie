% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_grclus}
\alias{morie_grclus}
\title{Multilevel k-way graph partitioning (METIS)}
\usage{
morie_grclus(A, k = 2L, weights = NULL, matching = "hem", initial = "gggp", refinement = "bkl", tolerance = 0.03, coarsest = 20L, seed = 17)
}
\arguments{
\item{A}{Square adjacency matrix (matrix or list of numeric vectors),   symmetric, non-negative weights, no self-loops, no non-finite   values.}
\item{k}{Number of parts.}
\item{weights}{Optional positive vertex weights.}
\item{matching}{One of \code{"hem"} (heavy edge, default), \code{"rm"}   (random) or \code{"lem"} (light edge).}
\item{initial}{One of \code{"gggp"} (greedy region growth, default) or   \code{"ggp"} (breadth-first).}
\item{refinement}{One of \code{"bkl"} (boundary KL, default) or   \code{"kl"} (all vertices).}
\item{tolerance}{Balance tolerance in \code{[0, 1)}.}
\item{coarsest}{Coarsening stops when the graph is this small.}
\item{seed}{Seed for the shared generator.}
}
\description{
Splits the vertices into \code{k} nearly-balanced parts whose between-part edge weight is as small as possible. Following Karypis & Kumar (1998) the graph is coarsened by matching (\code{"hem"}, \code{"rm"} or \code{"lem"}), cut while small, and the cut is carried back up and refined with KL or BKL at every level. \code{k} parts are obtained by recursive bisection.
}
\value{
A list with \code{estimate} (= edge-cut), \code{partition},   \code{edge_cut}, \code{k}, \code{sizes}, \code{part_weights},   \code{balance}, \code{total_edge_weight}, \code{cut_fraction},   \code{matching}, \code{initial}, \code{refinement}, \code{tolerance},   \code{n}, \code{method}, \code{note}.
}
\keyword{internal}
