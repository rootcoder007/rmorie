% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_dbscan_native}
\alias{.morie_dbscan_native}
\title{DBSCAN density-based clustering (R parity)}
\usage{
.morie_dbscan_native(x, eps = 0.5, min_samples = 5L, metric = "euclidean")
}
\arguments{
\item{x}{A numeric matrix, one row per point.}
\item{eps}{Neighbourhood radius.}
\item{min_samples}{Points within eps needed to make a point a core   point, counting the point itself.}
\item{metric}{One of \code{"euclidean"}, \code{"manhattan"},   \code{"chebyshev"}.}
}
\description{
Native implementation; the metric is honoured and the reported core points are the points meeting the min_samples rule.
}
\value{
A list with \code{labels} (0-based cluster ids, -1 for   noise) and \code{core} (logical, one per point).
}
\keyword{internal}
