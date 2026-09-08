% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{cosine_similarity}
\alias{cosine_similarity}
\title{cosine_similarity}
\usage{
cosine_similarity(u, v) 
}
\arguments{
\item{u}{Passed to \code{.sbert_vec}.}
\item{v}{Passed to \code{.sbert_vec}.}
}
\description{
A step of the sbert_native implementation. Called by \code{rank_by_similarity}, \code{sts_score}. See the file header for the source the module follows.
}
\value{
A numeric value.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
cosine_similarity(V, V)
}
