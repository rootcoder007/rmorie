% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.alfomg_shape}
\alias{.alfomg_shape}
\title{.alfomg_shape}
\usage{
.alfomg_shape(msa)
}
\arguments{
\item{msa}{A vector; its length is taken and its elements indexed.}
}
\description{
A step of the alfomg_native implementation. Called by \code{morie_alfomg}, \code{morie_alfomg_opm}, \code{morie_alfomg_row_attention}. See the file header for the source the module follows.
}
\value{
A vector, from \code{c}.
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
.alfomg_shape(V)
}
