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
\item{distances}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{n_neighbors}{Coerced to integer by the body, with \code{as.integer}.}
\item{rho}{Optional; may be \code{NULL}. Numeric; combined arithmetically in the body.}
\item{tol}{Passed to \code{<}. Defaults to \code{1e-05}.}
\item{max_iter}{Coerced to integer by the body, with \code{as.integer}. Defaults to \code{64}.}
\item{min_scale}{Numeric; combined arithmetically in the body. Defaults to \code{0.001}.}
}
\description{
A step of the scumap_native implementation. Called by \code{morie_scumap_fuzzy_simplicial_set}. See the file header for the source the module follows.
}
\value{
A list with \code{sigma}, \code{rho}.
}
\keyword{internal}
\examples{
morie_scumap_smooth_knn_dist(distances = c(1, 2, 3, 4, 5, 6, 7, 8), n_neighbors = 5L)
}
