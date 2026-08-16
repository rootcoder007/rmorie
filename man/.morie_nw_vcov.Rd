% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_nw_vcov}
\alias{.morie_nw_vcov}
\title{Internal helper: Newey-West (Bartlett) HAC covariance for OLS}
\usage{
.morie_nw_vcov(X, resid, lag = NULL)
}
\description{
Internal helper: Newey-West (Bartlett) HAC covariance for OLS
}
\value{
The value of \code{\%*\%}.
}
\keyword{internal}
\examples{
M <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 2)
.morie_nw_vcov(M, M)
}
