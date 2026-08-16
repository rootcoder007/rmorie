% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{sample_acf}
\alias{sample_acf}
\title{sample_acf}
\usage{
sample_acf(x, lags)
}
\arguments{
\item{x}{A vector; its length is taken and its elements indexed.}
\item{lags}{See Usage.}
}
\description{
A step of the sarima_native implementation. Called by \code{preliminary_estimates}. See the file header for the source the module follows.
}
\value{
The value of \code{out}, as built in the body.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
M <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 2)
sample_acf(V, M)
}
