% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_dbscan_clustering}
\alias{morie_dbscan_clustering}
\title{DBSCAN density-based clustering (R parity)}
\usage{
morie_dbscan_clustering(x, eps = 0.5, min_samples = 5L, metric = "euclidean")
}
\arguments{
\item{x}{Numeric matrix, one row per point (a vector is treated as   one column).}
\item{eps}{Neighbourhood radius.}
\item{min_samples}{Points within \code{eps} needed to make a point a   core point, counting the point itself.}
\item{metric}{One of \code{"euclidean"} (default),   \code{"manhattan"} or \code{"chebyshev"}. Anything else is   refused.}
}
\description{
Native implementation: the metric the caller asks for is the metric used, and the core points reported are the points that meet the min_samples rule -- not every clustered point, which is what the previous wrapper reported.
}
\value{
Named list: \code{estimate} (the cluster count),   \code{labels} (0-based, \code{-1} for noise), \code{n_clusters},   \code{n_noise}, \code{core_sample_indices} (0-based, the core   points only), \code{eps}, \code{min_samples}, \code{metric},   \code{n} and \code{method}.
}
\keyword{internal}
\examples{
x <- rbind(matrix(rnorm(80, 0, 0.2), ncol = 2), matrix(rnorm(80, 
    5, 0.2), ncol = 2))
morie_dbscan_clustering(x, eps = 0.6, min_samples = 4L)
}
