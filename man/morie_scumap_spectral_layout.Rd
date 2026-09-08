% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_scumap_spectral_layout}
\alias{morie_scumap_spectral_layout}
\title{morie_scumap_spectral_layout}
\usage{
morie_scumap_spectral_layout(B, n_components = 2, laplacian = "normalised") 
}
\arguments{
\item{B}{A matrix; indexed by row and column.}
\item{n_components}{Coerced to integer by the body, with \code{as.integer}. Defaults to \code{2}.}
\item{laplacian}{One of \code{"as_printed"}, \code{"normalised"}. Defaults to \code{"normalised"}.}
}
\description{
A step of the scumap_native implementation. Called by \code{morie_scumap_umap_singlecell}. See the file header for the source the module follows.
}
\value{
The value of \code{Y}, as built in the body.
}
\keyword{internal}
\examples{
M <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 2)
morie_scumap_spectral_layout(M)
}
