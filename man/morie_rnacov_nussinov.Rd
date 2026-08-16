% Generated from the module source by ledger/wave3/gen_rd_any.R --
% roxygen2 hangs on this package. Usage comes from args(), so it
% tracks the formals.
\name{morie_rnacov_nussinov}
\alias{morie_rnacov_nussinov}
\title{Maximum base pairing by the Nussinov dynamic program}
\usage{
morie_rnacov_nussinov(seq, min_loop = 3L)
}
\arguments{
\item{seq}{A sequence string.}
\item{min_loop}{The minimum hairpin loop.}
}
\description{
Fills the upper triangle with the best number of pairs on each subsequence, then traces back. The minimum loop length is a physical constraint, not a tuning knob: a hairpin cannot close on fewer than about three unpaired bases.
}
\value{
A list with the zero-based pair matrix and the pair count.
}
\references{
  Eddy, S.R. and Durbin, R. (1994) "RNA sequence analysis using
    covariance models." Nucleic Acids Research 22(11), 2079-2088.
    doi:10.1093/nar/22.11.2079.
  Nawrocki, E.P. and Eddy, S.R. (2013) "Infernal 1.1: 100-fold faster
    RNA homology searches." Bioinformatics 29(22), 2933-2935.
  Rivas, E., Clements, J. and Eddy, S.R. (2017) "A statistical test
    for conserved RNA structure shows lack of evidence for structure
    in lncRNAs." Nature Methods 14(1), 45-48.
  Nussinov, R. and Jacobson, A.B. (1980) "Fast algorithm for
    predicting the secondary structure of single-stranded RNA." PNAS
    77(11), 6309-6313.
  Miller, G.A. (1955) "Note on the bias of information estimates." In
    Information Theory in Psychology, 95-100.
}
\keyword{internal}
