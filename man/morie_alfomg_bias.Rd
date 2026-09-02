% Generated from the module source by ledger/wave3/gen_rd_any.R --
% roxygen2 hangs on this package. Usage comes from args(), so it
% tracks the formals.
\name{morie_alfomg_bias}
\alias{morie_alfomg_bias}
\title{The attention bias read off the pair representation}
\usage{
morie_alfomg_bias(pair, w = NULL) 
}
\arguments{
\item{pair}{The pair representation, a list of rows of vectors.}
\item{w}{The learned map, or NULL.}
}
\description{
In the trained network this is a linear map with no bias term from the pair channel down to one scalar per attention head. With no weights, the mean over the pair channels stands in for it -- that is this module's choice, not AlphaFold's, and it is the linear map with every weight equal to 1/c, which keeps the bias on the scale of the representation instead of growing with its width.
}
\value{
An r by r numeric matrix.
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
D <- data.frame(x = c(1, 2, 3, 4), y = c(2, 4, 5, 9))
morie_alfomg_bias(D)
}
