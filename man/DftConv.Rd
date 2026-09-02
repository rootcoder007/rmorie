% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{DftConv}
\alias{DftConv}
\title{Eq (3.87).  The book is explicit that the convolution here is}
\usage{
DftConv(x, h) 
}
\arguments{
\item{x}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{h}{Coerced to numeric by the body, with \code{as.numeric}.}
}
\description{
PERIODIC: multiplying N-point DFTs gives the circular convolution of eq (3.90), and the linear one needs L >= Nx + Nh - 1 with both sequences zero-padded.  Both are returned so the wrap is visible.
}
\value{
A list with \code{linear}, \code{circular}, \code{from_dft}, \code{padded_length}, \code{n_linear}, \code{n_circular}, \code{max_difference}, \code{holds}, \code{wraps_if_unpadded}, \code{method}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
DftConv(V, V)
}
