% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_quantize_with_codebook}
\alias{morie_quantize_with_codebook}
\title{Map each sample to its nearest codeword}
\usage{
morie_quantize_with_codebook(x, codebook) 
}
\arguments{
\item{x}{Samples.}
\item{codebook}{Codebook.}
}
\description{
Map each sample to its nearest codeword
}
\value{
A list with \code{estimate}, \code{indices}, \code{values},   \code{mse}, \code{levels}, \code{method}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
morie_quantize_with_codebook(V, V)
}
