% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{upsample}
\alias{upsample}
\title{upsample}
\usage{
upsample(grid, factor = 2)
}
\arguments{
\item{grid}{Passed to \code{.samdec_mat}.}
\item{factor}{Coerced to integer by the body, with \code{as.integer}. Defaults to \code{2}.}
}
\description{
A step of the samdec_native implementation. Called by \code{decode_mask}. See the file header for the source the module follows.
}
\value{
One of two values, depending on the branch taken.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
upsample(V)
}
