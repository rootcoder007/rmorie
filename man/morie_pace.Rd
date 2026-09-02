% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_pace}
\alias{morie_pace}
\title{Sparse functional PCA by conditional expectation (PACE)}
\usage{
morie_pace(Y, argvals, K = 2L, n_grid = 21L, bw_mu = NULL, bw_cov = NULL, 
    kernel = "epan", shrink = TRUE) 
}
\arguments{
\item{Y}{A list of numeric vectors, one per subject: the observed   values.}
\item{argvals}{A list of numeric vectors, the observation times, one   vector per subject and the same length as that subject\'s values.}
\item{K}{Number of components to keep.}
\item{n_grid}{Size of the shared grid the mean, covariance and   eigenfunctions are evaluated on.}
\item{bw_mu,bw_cov}{Bandwidths for the mean and covariance   smoothers. \code{NULL} takes a rule of thumb for the mean and one   and a half times it for the covariance, the surface being harder   to estimate.}
\item{kernel}{\code{"epan"} or \code{"gauss"}.}
\item{shrink}{If \code{TRUE}, scores are the conditional expectation   (the PACE estimator, which shrinks); if \code{FALSE} they are the   trapezoid integral of the centred curve against the eigenfunction,   which is unbiased but noisy when a subject has few points.}
}
\description{
Principal Analysis by Conditional Expectation: the method for functional data observed at few, irregular times per subject, where a curve cannot be estimated one subject at a time. The mean and the covariance surface are smoothed across ALL subjects\' observations pooled, the eigenfunctions come from that surface, and each subject\'s scores are the conditional expectation given their handful of points -- which is what shrinks a sparsely observed subject towards the mean rather than letting noise dominate. The measurement-error variance is the gap between the smoothed diagonal of the raw second moments and the smoothed off-diagonal surface extended to the diagonal: the diagonal carries error, the off-diagonal does not, so the difference identifies it. Pairs with \code{a == b} are excluded from the covariance fit for exactly that reason.
}
\value{
A list with \code{scores} (one vector per subject),   \code{eigenvalues}, \code{eigenfunctions} (on the grid),   \code{mean}, \code{grid}, \code{sigma2}, \code{fve},   \code{fitted}, \code{n}, \code{K}, \code{n_grid}, \code{bw_mu},   \code{bw_cov}, \code{kernel}, \code{shrink}, \code{n_obs} and   \code{method}.
}
\keyword{internal}
\examples{
set.seed(1)
Y <- list(); argvals <- list()
for (i in 1:25) {
  ti <- sort(runif(7))
  Y[[i]] <- sin(2 * pi * ti) + rnorm(7, 0, 0.2)
  argvals[[i]] <- ti
}
r <- morie_pace(Y, argvals, K = 2L, n_grid = 15L)
length(r$mu) == 15L
}
