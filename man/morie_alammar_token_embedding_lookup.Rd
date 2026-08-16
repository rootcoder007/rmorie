% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_alammar_token_embedding_lookup}
\alias{morie_alammar_token_embedding_lookup}
\title{Token lookup, masked pooling, contextual extraction (Alammar Ch 2/8)}
\usage{
morie_alammar_token_embedding_lookup(ids, E_tok)
}
\arguments{
\item{ids}{0-based token ids.}
\item{E_tok}{V x d embedding table.}
}
\description{
Token lookup, masked pooling, contextual extraction (Alammar Ch 2/8)
}
\value{
A list with \code{embeddings}, \code{estimate}, \code{vocab_size}, \code{dim}, \code{n}, \code{method}.
}
\keyword{internal}
