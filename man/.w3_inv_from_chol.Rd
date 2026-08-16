% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.w3_inv_from_chol}
\alias{.w3_inv_from_chol}
\title{(L L\\')^-1, column by column from the factor}
\usage{
.w3_inv_from_chol(lo)
}
\arguments{
\item{lo}{A matrix; passed to \code{nrow}.}
}
\description{
A step of the helpers_w3num implementation. Called by \code{.w3_ols}. See the file header for the source the module follows.
}
\value{
The value of \code{out}, as built in the body.
}
\keyword{internal}
\examples{
M <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 2)
.w3_inv_from_chol(M)
}
