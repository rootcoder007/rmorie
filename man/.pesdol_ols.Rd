% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.pesdol_ols}
\alias{.pesdol_ols}
\title{.pesdol_ols}
\usage{
.pesdol_ols(X, y)
}
\arguments{
\item{X}{A matrix; passed to \code{nrow}.}
\item{y}{A matrix; passed to \code{crossprod}.}
}
\description{
A step of the pesdol_native implementation. Called by \code{morie_pesdol_ardl_bounds}. See the file header for the source the module follows.
}
\value{
A list with \code{beta}, \code{fit}, \code{res}.
}
\keyword{internal}
\examples{
M <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 2)
.pesdol_ols(M, M)
}
