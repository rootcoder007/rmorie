% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{predict_next}
\alias{predict_next}
\title{predict_next}
\usage{
predict_next(state, item_embeddings, top_k = 5, exclude = numeric(0))
}
\arguments{
\item{state}{See Usage.}
\item{item_embeddings}{See Usage.}
\item{top_k}{Defaults to \code{5}.}
\item{exclude}{Defaults to \code{numeric(0)}.}
}
\description{
Part of the sasRec_native implementation; see the file header for the source it follows.
}
\value{
A list with \code{estimate}, \code{ranking}, \code{n_scored}, \code{method}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
M <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 2)
predict_next(V, M)
}
