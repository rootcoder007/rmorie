% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{variation_norm}
\alias{variation_norm}
\title{variation_norm}
\usage{
variation_norm(beta)
}
\arguments{
\item{beta}{Coerced to numeric by the body, with \code{as.numeric}.}
}
\description{
A step of the tlhal_native implementation. Called by \code{hal_fit}, \code{morie_tlhal}. See the file header for the source the module follows.
}
\value{
A numeric value.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
variation_norm(V)
}
