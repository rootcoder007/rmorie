% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{CnnSig}
\alias{CnnSig}
\title{Section 10.8.2 names CNNs as the common deep model but gives no layer}
\usage{
CnnSig(x, kernels, bias = NULL, pool = 2, dense = NULL) 
}
\arguments{
\item{x}{A vector; its length is taken and its elements indexed.}
\item{kernels}{Passed to \code{.morie_bx_mat}.}
\item{bias}{Optional; may be \code{NULL}. Passed to \code{.morie_bx_vec}.}
\item{pool}{Numeric; combined arithmetically in the body. Defaults to \code{2}.}
\item{dense}{Optional; may be \code{NULL}. Passed to \code{.morie_bx_mat}.}
}
\description{
equations; the convolution, rectifier and pooling used here are those of LeCun, Bengio and Hinton, Nature 521(7553):436-444, 2015, which is reference [35] of that section.
}
\value{
A list with \code{maps}, \code{pooled}, \code{features}, \code{scores}, \code{predicted}, \code{method}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
CnnSig(V, V)
}
