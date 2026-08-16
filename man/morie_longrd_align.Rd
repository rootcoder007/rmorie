% Generated from the module source by ledger/wave3/gen_rd_any.R --
% roxygen2 hangs on this package. Usage comes from args(), so it
% tracks the formals.
\name{morie_longrd_align}
\alias{morie_longrd_align}
\title{Needleman-Wunsch global alignment, with the traceback written out}
\usage{
morie_longrd_align(a, b, match = 1, mismatch = -1, gap = -2)
}
\arguments{
\item{a,b}{The two sequences.}
\item{match,mismatch,gap}{The scoring.}
}
\description{
Returns the score and the two gapped strings. Ties are broken in a fixed order -- diagonal, then up, then left -- so the alignment is a function of its arguments. Any other order gives an alignment of the same score and a different pileup, which is why the order is stated rather than left to whichever branch ran first.
}
\value{
A list with the score and the two gapped sequences.
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
