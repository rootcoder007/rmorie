% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{phi}
\alias{phi}
\title{phi}
\usage{
phi(x, fields, W) 
}
\arguments{
\item{x}{See Usage.}
\item{fields}{A vector; indexed elementwise.}
\item{W}{A vector; indexed elementwise.}
}
\description{
A step of the ffmFM_native implementation. Called by \code{explor}, \code{fit_ffm}, \code{gated_update} and 15 others in the module. See the file header for the source the module follows.
}
\value{
The value of \code{tot}, as built in the body.
}
\keyword{internal}
\examples{
set.seed(3)
W <- lapply(1:4, function(j) lapply(1:2, function(f) rnorm(3, 0, 0.3)))
x <- list(list(0L, 1.0), list(2L, 1.0))
phi(x, fields = c(0L, 0L, 1L, 1L), W)
}
