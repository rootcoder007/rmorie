% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.pibmd_ecdf}
\alias{.pibmd_ecdf}
\title{.pibmd_ecdf}
\usage{
.pibmd_ecdf(sorted_v, x)
}
\arguments{
\item{sorted_v}{A vector; its length is taken.}
\item{x}{Passed to \code{<=}.}
}
\description{
A step of the pibmd_native implementation. Called by \code{morie_pibmd_prior_informativeness_bias_diagnostic}. See the file header for the source the module follows.
}
\value{
A numeric value.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.pibmd_ecdf(V, V)
}
