% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.dssm_to_vec}
\alias{.dssm_to_vec}
\title{Vectorise a "vec" coercion: an R numeric/integer, a list of numbers,}
\usage{
.dssm_to_vec(x)
}
\arguments{
\item{x}{See Usage.}
}
\description{
or anything that as.numeric() can flatten. Mirrors k.vec() on the Python side which is forgiving about list/array inputs.
}
\value{
A vector, from \code{as.numeric}.
}
\references{
----------
Huang, P.-S., He, X., Gao, J., Deng, L., Acero, A. & Heck, L. (2013)
"Learning Deep Structured Semantic Models for Web Search using
Clickthrough Data", *Proceedings of the 22nd ACM International
Conference on Information and Knowledge Management (CIKM \\'13)*,
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
