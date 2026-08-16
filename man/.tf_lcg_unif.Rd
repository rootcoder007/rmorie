% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.tf_lcg_unif}
\alias{.tf_lcg_unif}
\title{(state >> 11) is exact in a double: it is at most 2^53 - 1}
\usage{
.tf_lcg_unif(st)
}
\arguments{
\item{st}{A vector; indexed elementwise.}
}
\description{
A step of the rangayyan_tf implementation. Called by \code{EmdEns}. See the file header for the source the module follows.
}
\value{
A numeric value.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.tf_lcg_unif(V)
}
