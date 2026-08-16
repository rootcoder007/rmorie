% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_funCA_functional_cca}
\alias{morie_funCA_functional_cca}
\title{morie_funCA_functional_cca}
\usage{
morie_funCA_functional_cca(X, Y, p = NULL, q = NULL)
}
\arguments{
\item{X}{A matrix; passed to \code{as.matrix}.}
\item{Y}{A matrix; passed to \code{as.matrix}.}
\item{p}{Defaults to \code{NULL}.}
\item{q}{Defaults to \code{NULL}.}
}
\description{
A step of the funCA_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{estimate}, \code{correlations}, \code{weights_x}, \code{weights_y}, \code{variates_x}, \code{variates_y}, \code{p}, \code{q}, \code{explained_x}, \code{explained_y}, \code{eigenvalues_x}, \code{eigenvalues_y}, \code{n}, \code{method}, \code{note}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
morie_funCA_functional_cca(V, V)
}
