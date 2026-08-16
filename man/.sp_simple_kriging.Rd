% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.sp_simple_kriging}
\alias{.sp_simple_kriging}
\title{Internal: simple kriging solution (Sec 5.2.1).}
\usage{
.sp_simple_kriging(coords, z, target, cov_model = NULL, mu = NULL)
}
\arguments{
\item{coords}{Observation coordinates (n by d).}
\item{z}{Observed values, length n.}
\item{target}{Prediction locations (m by d).}
\item{cov_model}{Covariance spec.}
\item{mu}{Known mean; sample mean of `z` when NULL.}
}
\description{
lambda = Sigma^-1 sigma; p = mu + sigma\' Sigma^-1 (Z - mu) (5.10); variance = sigma^2 - sigma\' Sigma^-1 sigma (5.11).
}
\value{
List with prediction, variance, weights, mu.
}
\keyword{internal}
\examples{
.sp_simple_kriging(coords = c(1, 2, 3, 4, 5, 6, 7, 8), z = c(1, 2, 3, 4, 5, 6, 7, 8), target = c(1, 2, 3, 4, 5, 6, 7, 8))
}
