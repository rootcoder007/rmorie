% Generated from the module source by ledger/wave3/gen_rd_any.R --
% roxygen2 hangs on this package. Usage comes from args(), so it
% tracks the formals.
\name{morie_metsem_graph}
\alias{morie_metsem_graph}
\title{Nodes are the k-1-mers, edges the k-mers, weights the counts}
\usage{
morie_metsem_graph(reads, k) 
}
\arguments{
\item{reads}{The reads.}
\item{k}{The k-mer length.}
}
\description{
Reads shorter than k contribute nothing and are counted rather than dropped in silence: a run where most reads are shorter than the chosen k has a k problem, and the caller should be told.
}
\value{
A list with the edge weights, the adjacency, the nodes and   the read counts.
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
morie_metsem_graph(reads = data.frame(x = c(1, 2, 3, 4), y = c(2, 4, 5, 9)), k = 5L)
}
