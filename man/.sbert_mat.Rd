% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.sbert_mat}
\alias{.sbert_mat}
\title{.sbert_mat}
\usage{
.sbert_mat(x)
}
\arguments{
\item{x}{A matrix; the body checks with \code{is.matrix}.}
}
\description{
A step of the sbert_native implementation. Called by \code{pool}, \code{rank_by_similarity}. See the file header for the source the module follows.
}
\value{
Nothing; this branch always raises.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.sbert_mat(V)
}
