% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.sp_empirical_variogram}
\alias{.sp_empirical_variogram}
\title{Internal: empirical semivariogram by the method of moments.}
\usage{
.sp_empirical_variogram(coords, z, n_bins = 15, max_dist = NULL)
}
\arguments{
\item{coords}{Coordinate matrix (n by d).}
\item{z}{Numeric vector of length n.}
\item{n_bins}{Number of lag bins.}
\item{max_dist}{Largest lag retained; default half the maximum pair distance.}
}
\description{
gamma_hat(h) = 1 / (2 |N(h)|) * sum (Z(s_i) - Z(s_j))^2
}
\value{
List with lag, gamma, n_pairs.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.sp_empirical_variogram(V, V)
}
