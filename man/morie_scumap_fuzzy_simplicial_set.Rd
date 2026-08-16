% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_scumap_fuzzy_simplicial_set}
\alias{morie_scumap_fuzzy_simplicial_set}
\title{morie_scumap_fuzzy_simplicial_set}
\usage{
morie_scumap_fuzzy_simplicial_set(X, n_neighbors = 15, symmetrize = TRUE)
}
\arguments{
\item{X}{Passed to \code{.scumap_matrix}.}
\item{n_neighbors}{Coerced to integer by the body, with \code{as.integer}. Defaults to \code{15}.}
\item{symmetrize}{A flag; the body branches on it. Defaults to \code{TRUE}.}
}
\description{
A step of the scumap_native implementation. Called by \code{morie_scumap_umap_singlecell}. See the file header for the source the module follows.
}
\value{
A list with \code{A}, \code{B}, \code{rho}, \code{sigma}, \code{neighbours}, \code{n}.
}
\keyword{internal}
