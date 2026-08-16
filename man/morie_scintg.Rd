% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_scintg}
\alias{morie_scintg}
\title{morie_scintg}
\usage{
morie_scintg(Z, batches, K = NULL, sigma = 0.1, theta = 2, lam = 1, max_iter = 10, cluster_iter = 25, tol = 1e-04, seed = 0, reference = NULL, diversity = "penalise")
}
\arguments{
\item{Z}{Passed to \code{.scintg_matrix}.}
\item{batches}{A vector; its length is taken.}
\item{K}{Passed to \code{.scintg_maximum_diversity_clustering}.}
\item{sigma}{Passed to \code{.scintg_maximum_diversity_clustering}. Defaults to \code{0.1}.}
\item{theta}{Passed to \code{.scintg_maximum_diversity_clustering}. Defaults to \code{2}.}
\item{lam}{Passed to \code{.scintg_correct_batch}. Defaults to \code{1}.}
\item{max_iter}{Coerced to integer by the body, with \code{as.integer}. Defaults to \code{10}.}
\item{cluster_iter}{Passed to \code{.scintg_maximum_diversity_clustering}. Defaults to \code{25}.}
\item{tol}{Defaults to \code{1e-04}.}
\item{seed}{Passed to \code{.scintg_maximum_diversity_clustering}. Defaults to \code{0}.}
\item{reference}{Passed to \code{.scintg_correct_batch}.}
\item{diversity}{Passed to \code{.scintg_maximum_diversity_clustering}. Defaults to \code{"penalise"}.}
}
\description{
A step of the scintg_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{estimate}, \code{embedding}, \code{R}, \code{Y}, \code{K}, \code{objective}, \code{history}, \code{n_rounds}, \code{theta}, \code{sigma}, \code{lam}, \code{diversity}, \code{method}, \code{note}.
}
\keyword{internal}
\examples{
M <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 2)
D <- data.frame(x = c(1, 2, 3, 4), y = c(2, 4, 5, 9))
morie_scintg(M, D)
}
