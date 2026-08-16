% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.scumap_matrix}
\alias{.scumap_matrix}
\title{.scumap_matrix}
\usage{
.scumap_matrix(X)
}
\arguments{
\item{X}{A matrix; passed to \code{as.matrix}.}
}
\description{
A step of the scumap_native implementation. Called by \code{morie_scumap_fuzzy_simplicial_set}, \code{morie_scumap_umap_singlecell}. See the file header for the source the module follows.
}
\value{
The value of \code{M}, as built in the body.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.scumap_matrix(V)
}
