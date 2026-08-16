% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_bx_mean}
\alias{.morie_bx_mean}
\title{.morie_bx_mean}
\usage{
.morie_bx_mean(v)
}
\arguments{
\item{v}{A vector; its length is taken.}
}
\description{
A step of the rangayyan_class2 implementation. Called by \code{.morie_bx_kurt}, \code{.morie_bx_sd}, \code{EegBands} and 5 others in the module. See the file header for the source the module follows.
}
\value{
A numeric value.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.morie_bx_mean(V)
}
