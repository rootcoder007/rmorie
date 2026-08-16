% Generated from the module source by ledger/wave3/gen_rd_any.R --
% roxygen2 hangs on this package. Usage comes from args(), so it
% tracks the formals.
\name{morie_rnacov_mi}
\alias{morie_rnacov_mi}
\title{Mutual information between two alignment columns, in bits}
\usage{
morie_rnacov_mi(alignment, i, j, correction = "none")
}
\arguments{
\item{alignment}{A character vector of aligned sequences.}
\item{i}{First column, one-based.}
\item{j}{Second column, one-based.}
\item{correction}{"none" or "miller_madow".}
}
\description{
Zero for independent columns and zero when either column is constant. The second is the point: a perfectly conserved pair is perfectly uninformative about whether the positions interact.
}
\value{
A list with the information, the support and the cells seen.
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
