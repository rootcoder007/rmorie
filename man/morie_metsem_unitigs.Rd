% Generated from the module source by ledger/wave3/gen_rd_any.R --
% roxygen2 hangs on this package. Usage comes from args(), so it
% tracks the formals.
\name{morie_metsem_unitigs}
\alias{morie_metsem_unitigs}
\title{Maximal non-branching paths, plus the pure cycles}
\usage{
morie_metsem_unitigs(g) 
}
\arguments{
\item{g}{A graph from the graph builder.}
}
\description{
A branch point is any node that is not exactly one-in one-out. Every walk starts at an edge leaving one. What is left over after that is made only of one-in one-out nodes, which means it is a cycle; those are picked up separately so a circular replicon does not vanish for want of a place to start.
}
\value{
A list of unitigs, longest first.
}
\references{
  Nurk, S., Meleshko, D., Korobeynikov, A. and Pevzner, P.A. (2017)
    "metaSPAdes: a new versatile metagenomic assembler." Genome
    Research 27(5), 824-834. doi:10.1101/gr.213959.116.
  Pevzner, P.A., Tang, H. and Waterman, M.S. (2001) "An Eulerian path
    approach to DNA fragment assembly." PNAS 98(17), 9748-9753.
  Bankevich, A. et al. (2012) "SPAdes: a new genome assembly
    algorithm and its applications to single-cell sequencing."
    Journal of Computational Biology 19(5), 455-477.
    doi:10.1089/cmb.2012.0021.
}
\keyword{internal}
\examples{
ABUNDANT <- "ACGTACGGTCAGTTGCAAGT"
RARE <- "TTGCCATGAACCGGTTAACA"
READS <- c(rep(ABUNDANT, 4L), RARE)
K <- 7L
g <- morie_metsem_graph(READS, K)
morie_metsem_unitigs(g)
}
