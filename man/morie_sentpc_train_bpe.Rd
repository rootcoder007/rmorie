% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_sentpc_train_bpe}
\alias{morie_sentpc_train_bpe}
\title{Train a greedy BPE model}
\usage{
morie_sentpc_train_bpe(corpus, vocab_size, add_prefix = TRUE) 
}
\arguments{
\item{corpus}{Character vector of training sentences.}
\item{vocab_size}{Integer target vocabulary size.}
\item{add_prefix}{Logical; escape-and-prefix before counting.}
}
\description{
Merge the most frequent adjacent pair until the vocabulary budget is met. Greedy and deterministic: the merge list fully determines every future segmentation.
}
\value{
List with \code{merges}, \code{vocab}, \code{vocab_size},   \code{requested}, \code{algorithm}, \code{note}.
}
\keyword{internal}
\examples{
morie_sentpc_train_bpe(corpus = c(1, 2, 3, 4, 5, 6, 7, 8), vocab_size = 5L)
}
