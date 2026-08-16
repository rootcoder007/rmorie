% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.pibmd_moments}
\alias{.pibmd_moments}
\title{.pibmd_moments}
\usage{
.pibmd_moments(v)
}
\arguments{
\item{v}{A vector; its length is taken.}
}
\description{
A step of the pibmd_native implementation. Called by \code{.pibmd_bandwidth}, \code{morie_pibmd_prior_informativeness_bias_diagnostic}. See the file header for the source the module follows.
}
\value{
A vector, from \code{c}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.pibmd_moments(V)
}
