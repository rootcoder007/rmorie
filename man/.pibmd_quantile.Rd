% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.pibmd_quantile}
\alias{.pibmd_quantile}
\title{Type-7 quantile of an already-sorted vector}
\usage{
.pibmd_quantile(sorted_v, u)
}
\arguments{
\item{sorted_v}{A vector; its length is taken and its elements indexed.}
\item{u}{Numeric; combined arithmetically in the body.}
}
\description{
A step of the pibmd_native implementation. Called by \code{.pibmd_bandwidth}, \code{morie_pibmd_prior_informativeness_bias_diagnostic}. See the file header for the source the module follows.
}
\value{
A numeric value.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.pibmd_quantile(V, V)
}
