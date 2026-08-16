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
\item{D}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{score}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{weights}{Optional; may be \code{NULL}. Coerced to numeric by the body, with \code{as.numeric}.}
}
\description{
A step of the tlheic_native implementation. Called by \code{morie_tlheic}, \code{verify_gradient}. See the file header for the source the module follows.
}
\value{
A numeric value.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
gradient_inner_product(V, V)
}
