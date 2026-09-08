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
\item{state}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{item_embeddings}{See Usage.}
\item{top_k}{Numeric; passed to \code{min}. Defaults to \code{5}.}
\item{exclude}{Passed to \code{unlist}. Defaults to \code{numeric(0)}.}
}
\description{
A step of the sasRec_native implementation. No other function in the package calls it. See the file header for the source the module follows.
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
