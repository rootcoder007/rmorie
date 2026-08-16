% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_clrgrf}
\alias{morie_clrgrf}
\title{morie_clrgrf}
\usage{
morie_clrgrf(y, X, clusters, at = NULL, n_trees = 200L, min_leaf = 5L, subsample_frac = 0.5, seed = 0L, unit = "cluster", level = 0.95, cluster_sampling = TRUE)
}
\arguments{
\item{y}{See Usage.}
\item{X}{See Usage.}
\item{clusters}{See Usage.}
\item{at}{Defaults to \code{NULL}.}
\item{n_trees}{Defaults to \code{200L}.}
\item{min_leaf}{Defaults to \code{5L}.}
\item{subsample_frac}{Defaults to \code{0.5}.}
\item{seed}{Defaults to \code{0L}.}
\item{unit}{Defaults to \code{"cluster"}.}
\item{level}{Defaults to \code{0.95}.}
\item{cluster_sampling}{Defaults to \code{TRUE}.}
}
\description{
Part of the clrgrf_native implementation; see the file header for the source it follows.
}
\value{
A list with \code{estimate}, \code{fitted}, \code{se}, \code{ci}, \code{variance}, \code{n}, \code{n_clusters}, \code{clusters_per_tree}, \code{clusters_subsampled}, \code{cluster_sizes}, \code{cluster_labels}, \code{unit}, \code{cluster_sampling}, \code{n_trees}, \code{level}, \code{method}.
}
\keyword{internal}
