% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_alammar_ner_token_head}
\alias{morie_alammar_ner_token_head}
\title{morie_alammar_ner_token_head}
\usage{
morie_alammar_ner_token_head(h_tokens, W, b, tags = NULL)
}
\arguments{
\item{h_tokens}{Token hidden states, one row each.}
\item{W,tags}{Weight matrix and 0-based tag targets (or NULL).}
}
\description{
morie_alammar_ner_token_head
}
\value{
A list with \code{probabilities}, \code{predicted_tags}, \code{cross_entropy}, \code{estimate}, \code{n}, \code{method}.
}
\keyword{internal}
\examples{
morie_alammar_ner_token_head(h_tokens = c(1, 2, 3, 4, 5, 6, 7, 8), W = 5L, b = 5L)
}
