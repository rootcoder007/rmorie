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
\item{X}{Passed to \code{.sctraj_matrix}.}
\item{labels}{Coerced to character by the body, with \code{as.character}.}
\item{cov}{One of \code{"diagonal"}, \code{"euclidean"}. Defaults to \code{"full"}.}
\item{weights}{Optional; may be \code{NULL}. A vector; its length is taken and its elements indexed.}
}
\description{
A step of the sctraj_native implementation. Called by \code{morie_sctraj}. See the file header for the source the module follows.
}
\value{
A list with \code{distances}, \code{clusters}, \code{centers}, \code{covariances}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
morie_sctraj_cluster_distances(V, V)
}
