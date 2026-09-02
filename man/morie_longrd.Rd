% Generated from the module source by ledger/wave3/gen_rd_any.R --
% roxygen2 hangs on this package. Usage comes from args(), so it
% tracks the formals.
\name{morie_longrd}
\alias{morie_longrd}
\title{Polish a draft assembly with the reads it was built from}
\usage{
morie_longrd(assembly, reads, method = "pileup", min_depth = 3L, min_frac = 0.5, 
    ins_frac = 0.5, match = 1, mismatch = -1, gap = -2, sort_reads = TRUE) 
}
\arguments{
\item{assembly}{The draft.}
\item{reads}{The reads, already known to belong to this contig.}
\item{method}{Either pileup or poa.}
\item{min_depth}{Below this many reads a position is left alone. The   count of positions this protected is reported.}
\item{min_frac}{A call needs this share of the column, or the draft   stands.}
\item{ins_frac}{An insertion needs this share of the depth to be   accepted.}
\item{match,mismatch,gap}{The alignment scoring.}
\item{sort_reads}{Whether the partial-order route sorts first.}
}
\description{
Polish a draft assembly with the reads it was built from
}
\value{
A list with the polished sequence, the per-position depth and   support, the run-length view of both sequences, and how much   changed.
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
morie_longrd(assembly = 5L, reads = c(1, 2, 3, 4, 5, 6, 7, 8))
}
