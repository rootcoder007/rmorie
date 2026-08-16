% Generated from the module source by ledger/wave3/gen_rd_any.R --
% roxygen2 hangs on this package. Usage comes from args(), so it
% tracks the formals.
\name{morie_alfomg_opm}
\alias{morie_alfomg_opm}
\title{MSA to pair: the outer product mean}
\usage{
morie_alfomg_opm(msa)
}
\arguments{
\item{msa}{The alignment, a list of sequences, each a list of   channel vectors.}
}
\description{
AlphaFold 2 Supplementary Algorithm 10. Returns an r by r list of length-(c*c) vectors, the outer product of the two positions' channel vectors averaged over the alignment. The published block applies a layer normalisation and two learned projections first and one more to the result; with no weights in hand those are the identity, and the caller who has them applies them on either side. Flattened in row-major order, so entry a*c+b is channel a of position i against channel b of position j.
}
\value{
A list of lists of numeric vectors.
}
\references{
  Jumper, J. et al. (2021) "Highly accurate protein structure
    prediction with AlphaFold." Nature 596, 583-589.
    doi:10.1038/s41586-021-03819-2. Supplementary Algorithms 7 and 10.
  Ahdritz, G. et al. (2022) "OpenFold: retraining AlphaFold2 yields
    new insights into its learning mechanisms and capacity for
    generalization." bioRxiv 2022.11.20.517210.
  Vaswani, A. et al. (2017) "Attention is all you need." NeurIPS 30,
    5998-6008.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
morie_alfomg_opm(V)
}
