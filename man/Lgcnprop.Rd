% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{Lgcnprop}
\alias{Lgcnprop}
\title{LightGCN layer combination (He et al. 2020)}
\usage{
Lgcnprop(A, E, K, alpha = NULL) 
}
\description{
LightGCN layer combination (He et al. 2020)
}
\arguments{
\item{A}{Argument \code{A}; see Usage.}
\item{E}{Argument \code{E}; see Usage.}
\item{K}{Argument \code{K}; see Usage.}
\item{alpha}{Argument \code{alpha}; see Usage.}
}
\value{
A list with \code{E}, \code{K}, \code{alpha}, \code{n_nodes}, \code{dim}.
}
\keyword{internal}
\examples{
A <- rbind(c(0, 1, 0), c(1, 0, 1), c(0, 1, 0))
E <- matrix(rnorm(6), 3, 2)
rmorie:::Lgcnprop(A, E, K = 2)
}
