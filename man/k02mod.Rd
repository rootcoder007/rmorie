% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{k02mod}
\alias{k02mod}
\title{k02mod}
\usage{
k02mod(A, comm) 
}
\arguments{
\item{A}{A matrix; passed to \code{as.matrix}.}
\item{comm}{A vector; indexed elementwise.}
}
\description{
A step of the k02util implementation. Called by \code{louv}, \code{sgtcoml}. See the file header for the source the module follows.
}
\value{
A numeric value.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
M <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 2)
k02mod(M, V)
}
