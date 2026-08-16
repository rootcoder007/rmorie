% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_rg_conv}
\alias{.morie_rg_conv}
\title{.morie_rg_conv}
\usage{
.morie_rg_conv(xs, hs)
}
\arguments{
\item{xs}{A vector; its length is taken and its elements indexed.}
\item{hs}{A vector; its length is taken and its elements indexed.}
}
\description{
A step of the rangayyan_xfrm implementation. Called by \code{CCepSum}, \code{ClogSum}, \code{ConvModel} and 13 others in the module. See the file header for the source the module follows.
}
\value{
A vector, from \code{vapply}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.morie_rg_conv(V, V)
}
