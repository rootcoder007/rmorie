% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_dssm_word_hash}
\alias{morie_dssm_word_hash}
\title{Word hashing: replace the bag-of-words with a bag of letter n-grams}
\usage{
morie_dssm_word_hash(words, n = 3L, vocabulary = NULL) 
}
\arguments{
\item{words}{character vector. The token stream.}
\item{n}{integer(1). Letter n-gram size. Default \code{3}.}
\item{vocabulary}{character vector or \code{NULL}. If \code{NULL}   (default) the n-grams present in \code{words} are themselves the   vocabulary; otherwise only n-grams that appear in this list count   toward the output dimension, and any others contribute to   \code{unseen_ngrams}. }
}
\description{
The input layer\'s width becomes the number of n-grams, not the size of the vocabulary -- and an unseen word still has a vector.
}
\value{
A named list with \code{vector} (numeric, the bag-of-ngrams   counts in the canonical order), \code{dimension}, \code{ngrams}   (raw counts keyed by n-gram string), \code{unseen_ngrams} (number   of n-grams that were not in the supplied vocabulary), and   \code{note}.
}
\references{
----------
Huang, P.-S., He, X., Gao, J., Deng, L., Acero, A. & Heck, L. (2013)
"Learning Deep Structured Semantic Models for Web Search using
Clickthrough Data", *Proceedings of the 22nd ACM International
Conference on Information and Knowledge Management (CIKM \'13)*,
2333-2338, doi:10.1145/2505515.2505665. Sec. 3: the deep structured
semantic model projecting queries and documents into a common
low-dimensional semantic space where relevance is computed by cosine
similarity; WORD HASHING based on letter n-grams, which reduces the
dimensionality of the bag-of-words term vectors (a 500K vocabulary to
roughly 30K letter trigrams) with a very low collision rate and gives
representations to out-of-vocabulary and misspelled words; the
training objective maximising the conditional likelihood of the
CLICKED documents given the query under a softmax over the clicked
document and randomly sampled unclicked documents, with a smoothing
factor gamma in the softmax; and the criticism of earlier latent
semantic models as trained on objective functions loosely related to
the retrieval task.

Deerwester, S., Dumais, S. T., Furnas, G. W., Landauer, T. K. &
Harshman, R. (1990) "Indexing by latent semantic analysis", *Journal
of the American Society for Information Science* 41(6), 391-407. The
unsupervised alternative being displaced.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
morie_dssm_word_hash(V)
}
