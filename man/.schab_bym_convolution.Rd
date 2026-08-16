% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.schab_bym_convolution}
\alias{.schab_bym_convolution}
\title{.schab_bym_convolution}
\usage{
.schab_bym_convolution(u, v)
}
\arguments{
\item{u}{A vector; its length is taken.}
\item{v}{A vector; its length is taken.}
}
\description{
A step of the schab_glmm_shared implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A numeric value.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.schab_bym_convolution(V, V)
}
