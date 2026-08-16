% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.lou_modularity}
\alias{.lou_modularity}
\title{Q = (1/2m) sum_ij (A_ij - gamma k_i k_j / 2m) delta(z_i, z_j)}
\usage{
.lou_modularity(A, z, resolution = 1)
}
\arguments{
\item{A}{A matrix; indexed by row and column.}
\item{z}{A vector; indexed elementwise.}
\item{resolution}{Numeric; combined arithmetically in the body. Defaults to \code{1}.}
}
\description{
A step of the comlou implementation. Called by \code{Comlou}. See the file header for the source the module follows.
}
\value{
A numeric value.
}
\keyword{internal}
