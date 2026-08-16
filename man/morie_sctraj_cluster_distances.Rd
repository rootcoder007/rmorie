% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_sctraj_cluster_distances}
\alias{morie_sctraj_cluster_distances}
\title{morie_sctraj_cluster_distances}
\usage{
morie_sctraj_cluster_distances(X, labels, cov = "full", weights = NULL)
}
\arguments{
\item{X}{See Usage.}
\item{labels}{See Usage.}
\item{cov}{Defaults to \code{"full"}.}
\item{weights}{Defaults to \code{NULL}.}
}
\description{
Part of the sctraj_native implementation; see the file header for the source it follows.
}
\value{
A list with \code{distances}, \code{clusters}, \code{centers}, \code{covariances}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
morie_sctraj_cluster_distances(V, V)
}
