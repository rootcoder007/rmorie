% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.mor_pq_reconstruct}
\alias{.mor_pq_reconstruct}
\title{.mor_pq_reconstruct}
\usage{
.mor_pq_reconstruct(levels, radius)
}
\arguments{
\item{levels}{A vector; its length is taken and its elements indexed.}
\item{radius}{See Usage.}
}
\description{
A step of the polqnt_native implementation. Called by \code{morie_polqnt}. See the file header for the source the module follows.
}
\value{
The value of \code{r}, as built in the body.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.mor_pq_reconstruct(V, V)
}
