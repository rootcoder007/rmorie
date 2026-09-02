% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_alammar_bag_of_words}
\alias{morie_alammar_bag_of_words}
\title{BoW, c-TF-IDF, BIO tagging, vocabulary overlap (Alammar Ch 1-5)}
\usage{
morie_alammar_bag_of_words(tokens, vocab) 
}
\arguments{
\item{tokens,vocab}{Character vectors.}
}
\description{
BoW, c-TF-IDF, BIO tagging, vocabulary overlap (Alammar Ch 1-5)
}
\value{
A list with \code{bow_vector}, \code{oov_count}, \code{estimate}, \code{vocab_size}, \code{n}, \code{method}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
morie_alammar_bag_of_words(V, V)
}
