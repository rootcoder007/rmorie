% Generated from the module source by ledger/wave3/gen_rd_any.R --
% roxygen2 hangs on this package. Usage comes from args(), so it
% tracks the formals.
\name{morie_alfomg_row_attention}
\alias{morie_alfomg_row_attention}
\title{Pair to MSA: row-wise attention with a pair bias}
\usage{
morie_alfomg_row_attention(msa, bias, scale = NULL, gate = NULL)
}
\arguments{
\item{msa}{The alignment.}
\item{bias}{The r by r bias matrix.}
\item{scale}{The attention temperature; NULL is one over root c.}
\item{gate}{A per-channel output multiplier, or NULL for ungated.}
}
\description{
AlphaFold 2 Supplementary Algorithm 7, one head. Within each sequence, every position attends over every position of the same sequence, with the pair bias added to the logit before the softmax. Query, key and value are the identity here for the reason given in the file header. Multi-head attention is this operation on a slice of the channel axis, so a caller wanting h heads calls it h times on the slices and concatenates -- there is no head-specific arithmetic to hide.
}
\value{
A list with the attention weights and the attended output.
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
S <- 3L
R <- 4L
C <- 3L
CZ <- 5L
MSA <- lapply(0:(S - 1L), function(k) lapply(0:(R - 1L), function(i) vapply(0:(C - 
    1L), function(a) ((k * 37 + i * 11 + a * 5)%%17)/8 - 1, numeric(1))))
PAIR <- lapply(0:(R - 1L), function(i) lapply(0:(R - 1L), function(j) vapply(0:(CZ - 
    1L), function(d) ((i * 13 + j * 7 + d * 3)%%11)/10 - 0.5, 
    numeric(1))))
WB <- vapply(0:(CZ - 1L), function(d) ((d * 5 + 3)%%7)/6 - 0.5, 
    numeric(1))
b1 <- morie_alfomg_bias(PAIR, WB)
d <- 0
morie_alfomg_row_attention(MSA, b1)
}
