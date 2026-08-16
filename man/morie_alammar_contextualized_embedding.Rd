% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_alammar_contextualized_embedding}
\alias{morie_alammar_contextualized_embedding}
\title{morie_alammar_contextualized_embedding}
\usage{
morie_alammar_contextualized_embedding(layer_outputs, layer_idx, position)
}
\arguments{
\item{layer_outputs}{A (layers x seq x dim) array.}
\item{layer_idx,position}{0-based indices, negatives from the end.}
}
\description{
morie_alammar_contextualized_embedding
}
\value{
A list with \code{embedding}, \code{context_varies}, \code{estimate}, \code{layer}, \code{position}, \code{n}, \code{method}.
}
\keyword{internal}
