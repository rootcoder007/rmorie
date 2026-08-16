% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.longrd_call}
\alias{.longrd_call}
\title{Ties go to the draft base if it is among the leaders, and otherwise}
\usage{
.longrd_call(col, draft_base, min_depth, min_frac)
}
\arguments{
\item{col}{A list; the body reads \code{$-}, \code{$A}, \code{$C}, \code{$G}, \code{$T} from it.}
\item{draft_base}{See Usage.}
\item{min_depth}{See Usage.}
\item{min_frac}{See Usage.}
}
\description{
to the first base in alphabetical order -- an arbitrary rule, but a STATED arbitrary rule, which is what makes the two arms agree.
}
\value{
The value of \code{list}.
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
