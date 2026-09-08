% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_rkhs_norm}
\alias{morie_rkhs_norm}
\title{morie_rkhs_norm}
\usage{
morie_rkhs_norm(beta, K) 
}
\description{
morie_rkhs_norm
}
\arguments{
\item{beta}{Argument \code{beta}; see Usage.}
\item{K}{Argument \code{K}; see Usage.}
}
\value{
A vector.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
rmorie:::morie_rkhs_norm(V, V)
}
