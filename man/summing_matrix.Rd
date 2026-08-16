% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{summing_matrix}
\alias{summing_matrix}
\title{summing_matrix}
\usage{
summing_matrix(groups, n_bottom)
}
\arguments{
\item{groups}{A vector; its length is taken and its elements indexed.}
\item{n_bottom}{A matrix; passed to \code{diag}.}
}
\description{
A step of the hierF_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
The value of \code{rbind}.
}
\keyword{internal}
\examples{
S <- summing_matrix(list(c(0L, 1L, 2L, 3L), c(0L, 1L), c(2L, 3L)), 4L)
S
}
