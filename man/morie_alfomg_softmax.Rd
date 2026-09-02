% Generated from the module source by ledger/wave3/gen_rd_any.R --
% roxygen2 hangs on this package. Usage comes from args(), so it
% tracks the formals.
\name{morie_alfomg_softmax}
\alias{morie_alfomg_softmax}
\title{Softmax, shifted by the maximum before exponentiating}
\usage{
morie_alfomg_softmax(logits) 
}
\arguments{
\item{logits}{A numeric vector.}
}
\description{
The shift is not a nicety. Attention logits carrying a large pair bias overflow the exponential without it, and the shift is exactly cancelled by the normalisation, so it costs nothing.
}
\value{
A vector of the same length summing to one.
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
morie_alfomg_softmax(V)
}
