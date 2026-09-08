% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{cmip_ensemble}
\alias{cmip_ensemble}
\title{cmip_ensemble}
\usage{
cmip_ensemble(models, weights) 
}
\arguments{
\item{models}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{weights}{Coerced to numeric by the body, with \code{as.numeric}.}
}
\description{
Part of the caCMIP_mixedcase_native implementation; see the file header for the source it follows.
}
\value{
A list with \code{estimate}, \code{n}, \code{method}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
cmip_ensemble(V, V)
}
