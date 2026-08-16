% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.dmlqs_directed_edges}
\alias{.dmlqs_directed_edges}
\title{.dmlqs_directed_edges}
\usage{
.dmlqs_directed_edges(adj)
}
\arguments{
\item{adj}{Passed to \code{.dmlqs_norm_adj}.}
}
\description{
A step of the dmlqs_native implementation. Called by \code{morie_dmlqs}. See the file header for the source the module follows.
}
\value{
The value of \code{out}, as built in the body.
}
\keyword{internal}
\examples{
M <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 2)
.dmlqs_directed_edges(M)
}
