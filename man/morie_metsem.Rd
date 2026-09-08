% Generated from the module source by ledger/wave3/gen_rd_any.R --
% roxygen2 hangs on this package. Usage comes from args(), so it
% tracks the formals.
\name{morie_metsem}
\alias{morie_metsem}
\title{Assemble a community from its reads}
\usage{
morie_metsem(reads, k, tip_length = NULL, tip_ratio = 0.2, bubble_ratio = 0.5, 
    rounds = 2L, min_length = NULL) 
}
\arguments{
\item{reads}{The reads.}
\item{k}{The k-mer length.}
\item{tip_length}{A dead-end shorter than this is a tip candidate.   NULL is twice k, the usual choice, stated rather than hidden.}
\item{tip_ratio}{A tip goes only if its coverage is below this   fraction of the best branch it leaves -- RELATIVE, which is the   whole point: an absolute cut-off deletes the rare organisms.}
\item{bubble_ratio}{The thinner side of a bubble goes if it is below   this fraction of the thicker.}
\item{rounds}{Cleaning passes.}
\item{min_length}{Contigs shorter than this are reported separately   rather than thrown away.}
}
\description{
Assemble a community from its reads
}
\value{
A list with the contigs longest first, their coverage, the   N50, and what the cleaning removed.
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
morie_metsem(reads = data.frame(x = c(1, 2, 3, 4), y = c(2, 4, 5, 9)), k = 5L)
}
