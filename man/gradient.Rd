% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{gradient}
\alias{gradient}
\title{D/d v_{i,f} of hat y, used in the SGD step --------------------}
\usage{
gradient(x, V, f, i) 
}
\arguments{
\item{x}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{V}{A vector; indexed elementwise.}
\item{f}{See Usage.}
\item{i}{See Usage.}
}
\description{
A step of the fmFM_native implementation. Called by \code{fit_fm}. See the file header for the source the module follows.
}
\value{
A numeric value.
}
\keyword{internal}
\examples{
set.seed(1)
V <- matrix(rnorm(12, 0, 0.3), 6, 2)
gradient(x = c(1, 0, 1, 0, 0, 0), V, f = 1, i = 1)
}
