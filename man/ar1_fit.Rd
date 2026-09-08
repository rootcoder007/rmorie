% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{ar1_fit}
\alias{ar1_fit}
\title{ar1_fit}
\usage{
ar1_fit(x) 
}
\arguments{
\item{x}{A vector; its length is taken and its elements indexed.}
}
\description{
A step of the andmnh_native implementation. Called by \code{alpha_ar1}. See the file header for the source the module follows.
}
\value{
A vector, from \code{c}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
ar1_fit(V)
}
