% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_burkov_add_k}
\alias{morie_burkov_add_k}
\title{Add-k smoothing (Burkov Ch 2)}
\usage{
morie_burkov_add_k(counts_ngram, counts_prefix, V, k = 0.5)
}
\arguments{
\item{counts_ngram,counts_prefix}{Counts.}
\item{V}{Vocabulary size.}
\item{k}{Pseudo-count, positive.}
}
\description{
Add-k smoothing (Burkov Ch 2)
}
\value{
A list with \code{estimate}, \code{count_ngram}, \code{count_prefix}, \code{vocab_size}, \code{k}, \code{n}, \code{method}.
}
\keyword{internal}
\examples{
morie_burkov_add_k(counts_ngram = 3, counts_prefix = 10, V = 100, k = 0.5)
}
