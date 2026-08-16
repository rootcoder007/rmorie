% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{gradient_inner_product}
\alias{gradient_inner_product}
\title{gradient_inner_product}
\usage{
gradient_inner_product(D, score, weights = NULL)
}
\arguments{
\item{D}{See Usage.}
\item{score}{See Usage.}
\item{weights}{Defaults to \code{NULL}.}
}
\description{
Part of the tlheic_native implementation; see the file header for the source it follows.
}
\value{
A numeric value.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
gradient_inner_product(V, V)
}
