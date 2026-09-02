% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_geron_optics}
\alias{morie_geron_optics}
\title{OPTICS: reachability ordering and cluster extraction (Geron Ch 8, hmopt)}
\usage{
morie_geron_optics(X, min_samples = 5, max_eps = Inf, eps_cluster = NULL) 
}
\arguments{
\item{X}{Data (n, d).}
\item{min_samples}{Core-distance neighbourhood size.}
\item{max_eps}{Largest radius considered.}
\item{eps_cluster}{Cut for extracting labels.}
}
\description{
OPTICS: reachability ordering and cluster extraction (Geron Ch 8, hmopt)
}
\value{
A list with \code{ordering}, \code{reachability}, \code{reachability_plot}, \code{core_distances}, \code{labels}, \code{n_clusters}, \code{eps_cluster}, \code{estimate}, \code{n}, \code{method}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
morie_geron_optics(V)
}
