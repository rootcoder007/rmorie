% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_asmnvr}
\alias{morie_asmnvr}
\title{De novo assembly via the de Bruijn graph and an Eulerian path}
\usage{
morie_asmnvr(reads, k = NULL, multiplicity = "set") 
}
\arguments{
\item{reads}{Character vector of reads (need not be equal length).}
\item{k}{Integer k-mer length, >= 2. NULL means the shortest read   length.}
\item{multiplicity}{"set" (default) gives one edge per DISTINCT   k-mer (the Idury-Waterman / SBH construction); "count" gives   one edge per OCCURRENCE for callers who already know their   multiplicities.}
}
\description{
Reads are broken into k-mers; vertices are (k-1)-mers and each k-mer is an edge. The overlap-layout-consensus paradigm asks for a Hamiltonian path (NP-complete); this asks for an Eulerian path (easy, by Hierholzer 1873). Repeat resolution via Eulerian superpaths and error correction are NOT implemented here -- a branching graph is reported ambiguous and only the unambiguous unitigs are returned, matching the Python arm\'s honesty about the boundary.
}
\value{
A named list with keys matching the Python payload:   estimate, sequence, path, contigs, unambiguous,   length_is_lower_bound, branching, n_kmers, n_vertices, graph,   k, multiplicity, note, method.
}
\keyword{internal}
\examples{
D <- data.frame(x = c(1, 2, 3, 4), y = c(2, 4, 5, 9))
morie_asmnvr(D)
}
