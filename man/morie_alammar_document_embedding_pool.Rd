% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_alammar_document_embedding_pool}
\alias{morie_alammar_document_embedding_pool}
\title{morie_alammar_document_embedding_pool}
\usage{
morie_alammar_document_embedding_pool(token_embeddings, attention_mask = NULL) 
}
\arguments{
\item{token_embeddings}{Token vectors, one row each.}
\item{attention_mask}{0/1 vector; padding must not dilute the mean.}
}
\description{
morie_alammar_document_embedding_pool
}
\value{
A list with \code{embedding}, \code{tokens_pooled}, \code{estimate}, \code{n}, \code{method}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
morie_alammar_document_embedding_pool(V)
}
