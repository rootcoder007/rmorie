% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{fasttext}
\alias{fasttext}
\title{fasttext}
\usage{
fasttext(corpus, dim = 50, n_min = 3, n_max = 6, window = 5, epochs = 5, lr = 0.05, negative = 5, min_count = 1, boundary = TRUE, whole_word = TRUE, hash_buckets = NULL, seed = 0)
}
\arguments{
\item{corpus}{Passed to \code{.as_docs}.}
\item{dim}{Coerced to integer by the body, with \code{as.integer}. Defaults to \code{50}.}
\item{n_min}{Coerced to integer by the body, with \code{as.integer}. Defaults to \code{3}.}
\item{n_max}{Coerced to integer by the body, with \code{as.integer}. Defaults to \code{6}.}
\item{window}{Coerced to integer by the body, with \code{as.integer}. Defaults to \code{5}.}
\item{epochs}{Coerced to integer by the body, with \code{as.integer}. Defaults to \code{5}.}
\item{lr}{Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{0.05}.}
\item{negative}{Coerced to integer by the body, with \code{as.integer}. Defaults to \code{5}.}
\item{min_count}{Defaults to \code{1}.}
\item{boundary}{Defaults to \code{TRUE}.}
\item{whole_word}{Defaults to \code{TRUE}.}
\item{hash_buckets}{Optional; may be \code{NULL}. Passed to \code{.gram_slot}.}
\item{seed}{Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{0}.}
}
\description{
A step of the fastxt_native implementation. Called by \code{morie_fastxt}. See the file header for the source the module follows.
}
\value{
A list with \code{estimate}, \code{vectors}, \code{vocab}, \code{index}, \code{ngrams}, \code{ngram_index}, \code{Z}, \code{context}, \code{loss_history}, \code{final_loss}, \code{oov}, \code{n_vocab}, \code{n_ngrams}, \code{dim}, \code{n_min}, \code{n_max}, \code{hash_buckets}, \code{method}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
fasttext(V)
}
