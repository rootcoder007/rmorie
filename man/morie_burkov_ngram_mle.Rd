% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_burkov_ngram_mle}
\alias{morie_burkov_ngram_mle}
\title{N-gram MLE probability (Burkov Ch 2)}
\usage{
morie_burkov_ngram_mle(counts_ngram, counts_prefix) 
}
\arguments{
\item{counts_ngram,counts_prefix}{Counts.}
}
\description{
N-gram MLE probability (Burkov Ch 2)
}
\value{
A list with \code{estimate}, \code{count_ngram}, \code{count_prefix}, \code{n}, \code{method}.
}
\keyword{internal}
\examples{
morie_burkov_ngram_mle(counts_ngram = 5L, counts_prefix = 5L)
}
