% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.pibmd_bandwidth}
\alias{.pibmd_bandwidth}
\title{Silverman\\'s rule of thumb, robustified by the interquartile range}
\usage{
.pibmd_bandwidth(v)
}
\arguments{
\item{v}{A vector; its length is taken.}
}
\description{
A step of the pibmd_native implementation. Called by \code{.pibmd_kl_kde}. See the file header for the source the module follows.
}
\value{
A numeric value.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.pibmd_bandwidth(V)
}
