% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_as241_poly}
\alias{.morie_as241_poly}
\title{.morie_as241_poly}
\usage{
.morie_as241_poly(coef, x)
}
\arguments{
\item{coef}{A vector; its length is taken and its elements indexed.}
\item{x}{A vector; its length is taken.}
}
\description{
A step of the rng_native implementation. Called by \code{.morie_normal_quantile}. See the file header for the source the module follows.
}
\value{
The value of \code{out}, as built in the body.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.morie_as241_poly(V, V)
}
