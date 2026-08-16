% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_scumap_smooth_knn_dist}
\alias{morie_scumap_smooth_knn_dist}
\title{morie_scumap_smooth_knn_dist}
\usage{
morie_scumap_smooth_knn_dist(distances, n_neighbors, rho = NULL, tol = 1e-05, max_iter = 64, min_scale = 0.001)
}
\arguments{
\item{distances}{See Usage.}
\item{n_neighbors}{See Usage.}
\item{rho}{Defaults to \code{NULL}.}
\item{tol}{Defaults to \code{1e-05}.}
\item{max_iter}{Defaults to \code{64}.}
\item{min_scale}{Defaults to \code{0.001}.}
}
\description{
Part of the scumap_native implementation; see the file header for the source it follows.
}
\value{
A list with \code{sigma}, \code{rho}.
}
\keyword{internal}
\examples{
morie_scumap_smooth_knn_dist(distances = c(1, 2, 3, 4, 5, 6, 7, 8), n_neighbors = 5L)
}
