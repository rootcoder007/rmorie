% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_km2_ngrams}
\alias{.morie_km2_ngrams}
\title{.morie_km2_ngrams}
\usage{
.morie_km2_ngrams(tokens, n)
}
\arguments{
\item{tokens}{A vector; its length is taken and its elements indexed.}
\item{n}{Numeric; combined arithmetically in the body.}
}
\description{
A step of the kamath_llm2_native implementation. Called by \code{morie_kamath_bleu_score}, \code{morie_kamath_ch8_rouge_n}, \code{morie_kamath_rouge_n}. See the file header for the source the module follows.
}
\value{
A vector, from \code{vapply}.
}
\keyword{internal}
\examples{
.morie_km2_ngrams(tokens = c(1, 2, 3, 4, 5, 6, 7, 8), n = 5L)
}
