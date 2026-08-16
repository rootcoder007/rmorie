% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_geron_randomized_pca}
\alias{morie_geron_randomized_pca}
\title{Randomized PCA via a power-iterated random projection sketch (Geron Ch 7, hmrpca)}
\usage{
morie_geron_randomized_pca(X, n_components, seed = 0, n_oversamples = 10, n_power_iter = 2)
}
\arguments{
\item{X}{Data (m, p).}
\item{n_components}{Components to recover.}
\item{seed,n_oversamples,n_power_iter}{Sketch controls.}
}
\description{
Randomized PCA via a power-iterated random projection sketch (Geron Ch 7, hmrpca)
}
\value{
A list with \code{components}, \code{scores}, \code{singular_values}, \code{explained_variance}, \code{explained_variance_ratio}, \code{spectral_gap}, \code{sketch_width}, \code{estimate}, \code{n}, \code{method}.
}
\keyword{internal}
\examples{
morie_geron_randomized_pca(matrix(c(1, 2, 3, 1, 2, 3), ncol = 2), 
    1)
}
