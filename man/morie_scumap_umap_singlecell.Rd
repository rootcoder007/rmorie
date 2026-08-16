% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_scumap_umap_singlecell}
\alias{morie_scumap_umap_singlecell}
\title{morie_scumap_umap_singlecell}
\usage{
morie_scumap_umap_singlecell(X, n_neighbors = 15, min_dist = 0.1, n_components = 2, n_epochs = 200, learning_rate = 1, spread = 1, negative_sample_rate = 5, init = "spectral", seed = 0, laplacian = "normalised", a = NULL, b = NULL)
}
\arguments{
\item{X}{See Usage.}
\item{n_neighbors}{Defaults to \code{15}.}
\item{min_dist}{Defaults to \code{0.1}.}
\item{n_components}{Defaults to \code{2}.}
\item{n_epochs}{Defaults to \code{200}.}
\item{learning_rate}{Defaults to \code{1}.}
\item{spread}{Defaults to \code{1}.}
\item{negative_sample_rate}{Defaults to \code{5}.}
\item{init}{Defaults to \code{"spectral"}.}
\item{seed}{Defaults to \code{0}.}
\item{laplacian}{Defaults to \code{"normalised"}.}
\item{a}{Defaults to \code{NULL}.}
\item{b}{Defaults to \code{NULL}.}
}
\description{
Part of the scumap_native implementation; see the file header for the source it follows.
}
\value{
A list with \code{estimate}, \code{embedding}, \code{graph}, \code{directed_graph}, \code{rho}, \code{sigma}, \code{neighbours}, \code{a}, \code{b}, \code{n_neighbors}, \code{min_dist}, \code{n_components}, \code{n_epochs}, \code{init}, \code{laplacian}, \code{n}, \code{method}, \code{note}.
}
\keyword{internal}
