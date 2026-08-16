% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_geron_pca_variance}
\alias{morie_geron_pca_variance}
\title{PCA variance accounting: cumulative ratio, threshold cut, probe check (Geron Ch 7, hmpcav)}
\usage{
morie_geron_pca_variance(X, n_components = NULL, threshold = 0.95, n_probes = 64, seed = 0)
}
\arguments{
\item{X}{Data (m, p).}
\item{n_components}{Optional cap passed through to PCA.}
\item{threshold}{Variance fraction to reach.}
\item{n_probes,seed}{Random-direction check controls.}
}
\description{
PCA variance accounting: cumulative ratio, threshold cut, probe check (Geron Ch 7, hmpcav)
}
\value{
A list with \code{explained_variance}, \code{explained_variance_ratio}, \code{cumulative}, \code{n_components_for_threshold}, \code{top_variance}, \code{probe_max}, \code{covariance}, \code{estimate}, \code{n}, \code{method}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
morie_geron_pca_variance(V)
}
