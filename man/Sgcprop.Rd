% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{Sgcprop}
\alias{Sgcprop}
\title{Simplified graph convolution propagation (Wu et al. 2019, SGC)}
\usage{
Sgcprop(A, X, K) 
}
\description{
Simplified graph convolution propagation (Wu et al. 2019, SGC)
}
\arguments{
\item{A}{Argument \code{A}; see Usage.}
\item{X}{Argument \code{X}; see Usage.}
\item{K}{Argument \code{K}; see Usage.}
}
\value{
A list with \code{X}, \code{K}, \code{n_nodes}, \code{dim}.
}
\keyword{internal}
\examples{
A <- rbind(c(0, 1, 0), c(1, 0, 1), c(0, 1, 0))
X <- matrix(1:6, 3, 2)
rmorie:::Sgcprop(A, X, K = 2)
}
