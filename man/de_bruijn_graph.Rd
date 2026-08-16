% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{de_bruijn_graph}
\alias{de_bruijn_graph}
\title{Build G(S, l) with l = k. Vertices are (k-1)-mers; each k-mer is}
\usage{
de_bruijn_graph(reads, k, multiplicity = "set")
}
\arguments{
\item{reads}{See Usage.}
\item{k}{See Usage.}
\item{multiplicity}{Defaults to \code{"set"}.}
}
\description{
an edge from its prefix to its suffix. multiplicity="set" gives one edge per distinct k-mer (the Idury-Waterman construction); "count" gives one per occurrence for callers who already know their multiplicities. Returns a list (edges, indeg, outdeg); edges/indeg/outdeg are environments keyed by vertex (string), paralleling the Python dict-of-lists + counter-dicts.
}
\value{
A list with \code{edges}, \code{indeg}, \code{outdeg}.
}
\keyword{internal}
