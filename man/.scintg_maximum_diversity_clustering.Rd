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
\item{Z}{See Usage.}
\item{batches}{See Usage.}
\item{K}{Defaults to \code{NULL}.}
\item{sigma}{Defaults to \code{0.1}.}
\item{theta}{Defaults to \code{2}.}
\item{max_iter}{Defaults to \code{25}.}
\item{tol}{Defaults to \code{1e-05}.}
\item{seed}{Defaults to \code{0}.}
\item{Y}{Defaults to \code{NULL}.}
\item{diversity}{Defaults to \code{"penalise"}.}
}
\description{
Part of the scintg_native implementation; see the file header for the source it follows.
}
\value{
A list with \code{R}, \code{Y}, \code{K}, \code{objective}.
}
\keyword{internal}
