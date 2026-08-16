% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.sctraj_matrix}
\alias{.sctraj_matrix}
\title{.sctraj_matrix}
\usage{
.sctraj_matrix(X)
}
\arguments{
\item{X}{A matrix; passed to \code{as.matrix}.}
}
\description{
A step of the sctraj_native implementation. Called by \code{morie_sctraj}, \code{morie_sctraj_cluster_distances}, \code{morie_sctraj_principal_curve}. See the file header for the source the module follows.
}
\value{
The value of \code{M}, as built in the body.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.sctraj_matrix(V)
}
