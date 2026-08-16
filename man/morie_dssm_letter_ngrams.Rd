% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_dssm_letter_ngrams}
\alias{morie_dssm_letter_ngrams}
\title{Letter n-grams of a bracketed word}
\usage{
morie_dssm_letter_ngrams(word, n = 3L, boundary = "#")
}
\arguments{
\item{word}{character(1). The input word; coerced via \code{as.character}   and lower-cased.}
\item{n}{integer(1). Length of the n-grams. Default \code{3}.}
\item{boundary}{character(1). Bracket character pasted on both sides   before sliding the window. Default \code{"#"}. }
}
\description{
Bracketing matters: it distinguishes a prefix from the same letters inside a word. With \code{boundary = "#"} and \code{n = 3} the word "good" becomes the trigrams of \code{"#good#"}.
}
\value{
A character vector of length \code{max(1, nchar(w) - n + 1)}.   When the bracketed word is shorter than \code{n}, the entire   bracketed string is returned as a single "n-gram".
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
