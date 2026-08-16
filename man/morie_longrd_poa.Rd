% Generated from the module source by ledger/wave3/gen_rd_any.R --
% roxygen2 hangs on this package. Usage comes from args(), so it
% tracks the formals.
\name{morie_longrd_poa}
\alias{morie_longrd_poa}
\title{Progressive partial-order consensus: the heaviest path}
\usage{
morie_longrd_poa(reads, match = 1, mismatch = -1, gap = -2, sort_reads = TRUE)
}
\arguments{
\item{reads}{The reads.}
\item{match,mismatch,gap}{The scoring.}
\item{sort_reads}{Whether to sort first.}
}
\description{
The first read is the seed. Each later read is aligned to the current consensus and every aligned column votes; the consensus is recomputed as the column-wise majority, which for a partial order graph built this way IS the heaviest path, because every node's weight is the number of reads passing through it. Progressive construction depends on the order the reads arrive. That is a property of the method and not an accident, so the reads are sorted first by default -- which does not make the answer order-INDEPENDENT, it makes it a function of the SET rather than of the sequence, and those are different guarantees.
}
\value{
The consensus sequence.
}
\references{
  Vaser, R., Sovic, I., Nagarajan, N. and Sikic, M. (2017) "Fast and
    accurate de novo genome assembly from long uncorrected reads."
    Genome Research 27(5), 737-746. doi:10.1101/gr.214270.116.
  Lee, C., Grasso, C. and Sharlow, M.F. (2002) "Multiple sequence
    alignment using partial order graphs." Bioinformatics 18(3),
    452-464. doi:10.1093/bioinformatics/18.3.452.
  Needleman, S.B. and Wunsch, C.D. (1970) "A general method applicable
    to the search for similarities in the amino acid sequence of two
    proteins." Journal of Molecular Biology 48(3), 443-453.
  Wick, R.R., Judd, L.M. and Holt, K.E. (2019) "Performance of neural
    network basecalling tools for Oxford Nanopore sequencing." Genome
    Biology 20, 129. doi:10.1186/s13059-019-1727-y.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
morie_longrd_poa(V)
}
