% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{clrgrf_cluster_index}
\alias{clrgrf_cluster_index}
\title{clrgrf_cluster_index}
\usage{
clrgrf_cluster_index(clusters)
}
\arguments{
\item{clusters}{See Usage.}
}
\description{
A step of the clrgrf_native implementation. Called by \code{morie_clrgrf}. See the file header for the source the module follows.
}
\value{
A list with \code{groups}, \code{labels}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
clrgrf_cluster_index(V)
}
