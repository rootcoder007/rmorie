% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{naive_variance}
\alias{naive_variance}
\title{naive_variance}
\usage{
naive_variance(ic) 
}
\arguments{
\item{ic}{Coerced to numeric by the body, with \code{as.numeric}.}
}
\description{
A step of the tlclust_native implementation. Called by \code{design_effect}, \code{morie_tlclust}. See the file header for the source the module follows.
}
\value{
A numeric value.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
naive_variance(V)
}
