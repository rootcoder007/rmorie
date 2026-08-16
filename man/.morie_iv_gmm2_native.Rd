% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_iv_gmm2_native}
\alias{.morie_iv_gmm2_native}
\title{Internal helper: two-step efficient GMM for linear IV}
\usage{
.morie_iv_gmm2_native(y, X, Z)
}
\description{
Step 1 is 2SLS; step 2 re-weights with the inverse of the heteroskedasticity-robust moment covariance. Returns the Hansen J statistic as a by-product.
}
\value{
A list with \code{beta}, \code{se}, \code{vcov}, \code{residuals}, \code{J}, \code{J_df}, \code{J_p}, \code{n}.
}
\keyword{internal}
