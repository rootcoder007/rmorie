% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.mor_pq_decompose}
\alias{.mor_pq_decompose}
\title{.mor_pq_decompose}
\usage{
.mor_pq_decompose(x)
}
\arguments{
\item{x}{See Usage.}
}
\description{
A step of the polqnt_native implementation. Called by \code{morie_polqnt}. See the file header for the source the module follows.
}
\value{
A list with \code{levels}, \code{radius}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.mor_pq_decompose(V)
}
