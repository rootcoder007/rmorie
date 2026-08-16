% Generated from the module source by ledger/wave3/gen_rd_any.R --
% roxygen2 hangs on this package. Usage comes from args(), so it
% tracks the formals.
\name{morie_metsem_n50}
\alias{morie_metsem_n50}
\title{The length at which half the assembly is in contigs that long}
\usage{
morie_metsem_n50(lengths)
}
\arguments{
\item{lengths}{The contig lengths.}
}
\description{
Sorted longest first, accumulate until the running total reaches half the assembly; the contig you were on is the answer. Reported for an empty assembly as zero rather than as an error, because an assembly with no contigs is a result and not a mistake.
}
\value{
An integer.
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
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
morie_metsem_n50(V)
}
