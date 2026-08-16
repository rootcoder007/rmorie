% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.wls_r}
\alias{.wls_r}
\title{.wls_r}
\usage{
.wls_r(X, y, w)
}
\arguments{
\item{X}{A matrix; passed to \code{as.matrix}.}
\item{y}{A matrix; passed to \code{crossprod}.}
\item{w}{Numeric; combined arithmetically in the body.}
}
\description{
A step of the bhltmsm_native implementation. Called by \code{fit_msm}. See the file header for the source the module follows.
}
\value{
A matrix, from \code{solve}.
}
\keyword{internal}
\examples{
.wls_r(X = c(1, 2, 3, 4, 5, 6, 7, 8), y = c(1, 2, 3, 4, 5, 6, 7, 8), w = c(1, 2, 3, 4, 5, 6, 7, 8))
}
