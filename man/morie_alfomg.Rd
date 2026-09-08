% Generated from the module source by ledger/wave3/gen_rd_any.R --
% roxygen2 hangs on this package. Usage comes from args(), so it
% tracks the formals.
\name{morie_alfomg}
\alias{morie_alfomg}
\title{One Evoformer MSA-pair head, both directions}
\usage{
morie_alfomg(msa, pair, w_bias = NULL, w_opm = NULL, scale = NULL, gate = NULL) 
}
\arguments{
\item{msa}{The alignment, indexed sequence, position, channel.}
\item{pair}{The pair representation, indexed i, j, channel.}
\item{w_bias}{The learned map from a pair vector to the attention   bias. NULL means the mean over the pair channels.}
\item{w_opm}{The learned projection from the c*c outer product back   to the pair width, one row per output channel. NULL means the pair   representation is returned unchanged and \code{pair_updated} is   FALSE -- because inventing that projection would be inventing the   model.}
\item{scale}{The attention temperature; NULL is one over root c.}
\item{gate}{A per-channel output gate; NULL is ungated.}
}
\description{
One Evoformer MSA-pair head, both directions
}
\value{
A list with the outer product mean, the attention bias and   weights, the updated alignment, and the pair representation.
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
morie_alfomg(msa = c(1, 2, 3, 4, 5, 6, 7, 8), pair = 5L)
}
