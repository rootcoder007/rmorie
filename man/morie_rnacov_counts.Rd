% Generated from the module source by ledger/wave3/gen_rd_any.R --
% roxygen2 hangs on this package. Usage comes from args(), so it
% tracks the formals.
\name{morie_rnacov_counts}
\alias{morie_rnacov_counts}
\title{Joint and marginal letter counts for two columns, gaps dropped}
\usage{
morie_rnacov_counts(alignment, i, j)
}
\arguments{
\item{alignment}{A character vector of aligned sequences.}
\item{i}{First column, one-based.}
\item{j}{Second column, one-based.}
}
\description{
A sequence gapped in either column contributes to neither margin. Counting it in one and not the other would make the marginals and the joint disagree about how many sequences there were, and the mutual information would stop being a mutual information.
}
\value{
A list with the joint counts, both margins and the count.
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
\examples{
morie_rnacov_counts(alignment = c(1, 2, 3, 4, 5, 6, 7, 8), i = c(1, 2, 3, 4, 5, 6, 7, 8), j = c(1, 2, 3, 4, 5, 6, 7, 8))
}
