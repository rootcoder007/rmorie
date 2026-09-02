% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_burkov_laplace}
\alias{morie_burkov_laplace}
\title{Laplace add-1 smoothing (Burkov Ch 2)}
\usage{
morie_burkov_laplace(counts_ngram, counts_prefix, V) 
}
\arguments{
\item{counts_ngram,counts_prefix}{Counts.}
\item{V}{Vocabulary size.}
}
\description{
Laplace add-1 smoothing (Burkov Ch 2)
}
\value{
A list with \code{estimate}, \code{count_ngram}, \code{count_prefix}, \code{vocab_size}, \code{n}, \code{method}.
}
\keyword{internal}
\examples{
morie_burkov_laplace(counts_ngram = 3, counts_prefix = 10, V = 100)
}
