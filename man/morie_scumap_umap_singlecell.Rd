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
\item{X}{Passed to \code{.scumap_matrix}.}
\item{n_neighbors}{Passed to \code{morie_scumap_fuzzy_simplicial_set}. Defaults to \code{15}.}
\item{min_dist}{Passed to \code{morie_scumap_fit_ab}. Defaults to \code{0.1}.}
\item{n_components}{Coerced to integer by the body, with \code{as.integer}. Defaults to \code{2}.}
\item{n_epochs}{A count; the body uses it as \code{seq_len(...)}. Defaults to \code{200}.}
\item{learning_rate}{Numeric; combined arithmetically in the body. Defaults to \code{1}.}
\item{spread}{Passed to \code{morie_scumap_fit_ab}. Defaults to \code{1}.}
\item{negative_sample_rate}{Coerced to integer by the body, with \code{as.integer}. Defaults to \code{5}.}
\item{init}{One of \code{"random"}, \code{"spectral"}. Defaults to \code{"spectral"}.}
\item{seed}{Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{0}.}
\item{laplacian}{Passed to \code{morie_scumap_spectral_layout}. Defaults to \code{"normalised"}.}
\item{a}{Optional; may be \code{NULL}. Numeric; combined arithmetically in the body.}
\item{b}{Optional; may be \code{NULL}. Numeric; combined arithmetically in the body.}
}
\description{
A step of the scumap_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{estimate}, \code{embedding}, \code{graph}, \code{directed_graph}, \code{rho}, \code{sigma}, \code{neighbours}, \code{a}, \code{b}, \code{n_neighbors}, \code{min_dist}, \code{n_components}, \code{n_epochs}, \code{init}, \code{laplacian}, \code{n}, \code{method}, \code{note}.
}
\keyword{internal}
