% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.scintg_maximum_diversity_clustering}
\alias{.scintg_maximum_diversity_clustering}
\title{.scintg_maximum_diversity_clustering}
\usage{
.scintg_maximum_diversity_clustering(Z, batches, K = NULL, sigma = 0.1, theta = 2, max_iter = 25, tol = 1e-05, seed = 0, Y = NULL, diversity = "penalise")
}
\arguments{
\item{Z}{Passed to \code{.scintg_matrix}.}
\item{batches}{A vector; its length is taken.}
\item{K}{Optional; may be \code{NULL}. A count; the body uses it as \code{seq_len(...)}.}
\item{sigma}{Numeric; combined arithmetically in the body. Defaults to \code{0.1}.}
\item{theta}{Numeric; combined arithmetically in the body. Defaults to \code{2}.}
\item{max_iter}{Coerced to integer by the body, with \code{as.integer}. Defaults to \code{25}.}
\item{tol}{Numeric; combined arithmetically in the body. Defaults to \code{1e-05}.}
\item{seed}{Passed to \code{.scintg_kmeans_init}. Defaults to \code{0}.}
\item{Y}{Optional; may be \code{NULL}. A matrix; passed to \code{as.matrix}.}
\item{diversity}{Compared against \code{"as_printed"}. Defaults to \code{"penalise"}.}
}
\description{
A step of the scintg_native implementation. Called by \code{morie_scintg}. See the file header for the source the module follows.
}
\value{
A list with \code{R}, \code{Y}, \code{K}, \code{objective}.
}
\keyword{internal}
\examples{
M <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 2)
D <- data.frame(x = c(1, 2, 3, 4), y = c(2, 4, 5, 9))
.scintg_maximum_diversity_clustering(M, D)
}
