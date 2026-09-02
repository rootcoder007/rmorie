% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_dssm_click_posterior}
\alias{morie_dssm_click_posterior}
\title{Clickthrough-trained softmax posterior}
\usage{
morie_dssm_click_posterior(query_vector, clicked_vector, unclicked_vectors, gamma = 10) 
}
\arguments{
\item{query_vector}{numeric. A query embedding.}
\item{clicked_vector}{numeric. The embedding of the clicked document.}
\item{unclicked_vectors}{list or matrix of numeric embeddings of the   sampled non-clicks. Each element must have the same width as   \code{query_vector}.}
\item{gamma}{numeric(1). Smoothing factor. Must be strictly positive.   Default \code{10.0}. }
}
\description{
Softmax over the clicked document and a sample of unclicked ones. \eqn{\gamma} sets how sharply the posterior concentrates; at \eqn{\gamma \to 0} every document is equally likely whatever the model learned.
}
\value{
A named list whose names match the Python   \code{RichResult} payload: \code{estimate}, \code{posterior_clicked},   \code{posterior}, \code{similarities}, \code{gamma}, \code{loss},   \code{n_negatives}, \code{method}, \code{note}.
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
morie_dssm_click_posterior(query_vector = 5L, clicked_vector = 5L, unclicked_vectors = c(1, 2, 3, 4, 5, 6, 7, 8))
}
