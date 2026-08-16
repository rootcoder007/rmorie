% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.sgflrt_inv}
\alias{.sgflrt_inv}
\title{.sgflrt_inv}
\usage{
.sgflrt_inv(L)
}
\arguments{
\item{L}{A matrix; passed to \code{nrow}.}
}
\description{
A step of the sgflrt_native implementation. Called by \code{morie_sgflrt_spatial_glmm_fit}. See the file header for the source the module follows.
}
\value{
The value of \code{M}, as built in the body.
}
\keyword{internal}
\examples{
M <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 2)
.sgflrt_inv(M)
}
