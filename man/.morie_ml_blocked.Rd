% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_ml_blocked}
\alias{.morie_ml_blocked}
\title{.morie_ml_blocked}
\usage{
.morie_ml_blocked(path, dirs, z, children)
}
\arguments{
\item{path}{A vector; its length is taken and its elements indexed.}
\item{dirs}{A vector; indexed elementwise.}
\item{z}{See Usage.}
\item{children}{Passed to \code{.morie_ml_desc}.}
}
\description{
A step of the causal_molak implementation. Called by \code{.morie_ml_dsep}. See the file header for the source the module follows.
}
\value{
A logical value.
}
\keyword{internal}
\examples{
.morie_ml_blocked(path = c(1, 2, 3, 4, 5, 6, 7, 8), dirs = c(1, 2, 3, 4, 5, 6, 7, 8), z = c(1, 2, 3, 4, 5, 6, 7, 8), children = c(1, 2, 3, 4, 5, 6, 7, 8))
}
