% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_t2_neigh}
\alias{.morie_t2_neigh}
\title{.morie_t2_neigh}
\usage{
.morie_t2_neigh(A, n)
}
\arguments{
\item{A}{A matrix; indexed by row and column.}
\item{n}{A count; the body uses it as \code{seq_len(...)}.}
}
\description{
A step of the tail2_t04 implementation. Called by \code{BipartSpec}, \code{SignlessL}. See the file header for the source the module follows.
}
\value{
The value of \code{lapply}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
M <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 2)
.morie_t2_neigh(M, V)
}
