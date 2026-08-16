% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_burkov_kneser_ney}
\alias{morie_burkov_kneser_ney}
\title{Kneser-Ney smoothing (Burkov Ch 2)}
\usage{
morie_burkov_kneser_ney(counts_ngram, counts_prefix, continuation_counts, d = 0.75)
}
\arguments{
\item{counts_ngram,counts_prefix}{Counts.}
\item{continuation_counts}{`c(n_types_after_prefix,   continuation_count_of_word, total_bigram_types)`.}
\item{d}{Absolute discount in (0, 1).}
}
\description{
Kneser-Ney smoothing (Burkov Ch 2)
}
\value{
A list with \code{estimate}, \code{discounted_mle}, \code{lambda}, \code{p_continuation}, \code{n}, \code{method}.
}
\keyword{internal}
\examples{
morie_burkov_kneser_ney(counts_ngram = 5L, counts_prefix = 5L, continuation_counts = c(1, 2, 3, 4, 5, 6, 7, 8))
}
