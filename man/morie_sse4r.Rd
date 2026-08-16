% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_sse4r}
\alias{morie_sse4r}
\title{morie_sse4r}
\usage{
morie_sse4r(sequence, user_embedding, item_table, attend = NULL, top_k = 3)
}
\arguments{
\item{sequence}{Passed to \code{.sse4r_personalise}.}
\item{user_embedding}{Passed to \code{.sse4r_personalise}.}
\item{item_table}{A matrix; passed to \code{as.matrix}.}
\item{attend}{Defaults to \code{NULL}.}
\item{top_k}{Coerced to integer by the body, with \code{as.integer}. Defaults to \code{3}.}
}
\description{
A step of the sse4r_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{estimate}, \code{top_k}, \code{scores}, \code{context}, \code{method}, \code{note}.
}
\references{
----------
Wu, L., Li, S., Hsieh, C.-J. & Sharpnack, J. (2020) "SSE-PT:
Sequential Recommendation Via Personalized Transformer", *Proceedings
of the 14th ACM Conference on Recommender Systems (RecSys \'20)*,
328-337, doi:10.1145/3383313.3412258; earlier circulated as "Temporal
Collaborative Ranking Via Personalized Transformer",
arXiv:1908.05435. The observation that existing self-attentive
sequential models are not personalised and that different users\' rating
patterns are treated alike; the personalised Transformer concatenating
a user embedding with each item embedding in the input sequence; and
the use of Stochastic Shared Embeddings regularisation, replacing an
embedding by another from the same table with a given probability
during training, to make the large per-user embedding table trainable.

Wu, L., Li, S., Hsieh, C.-J. & Sharpnack, J. (2019) "Stochastic
Shared Embeddings: Data-driven Regularization of Embedding Layers",
*NeurIPS 2019*, arXiv:1905.10630. The SSE regulariser itself.

Kang, W.-C. & McAuley, J. (2018) "Self-Attentive Sequential
Recommendation", *ICDM 2018*, 197-206, arXiv:1808.09781. SASRec, the
unpersonalised model being extended; implemented in :mod:`sasRec`.
}
\keyword{internal}
\examples{
morie_sse4r(sequence = c(1, 2, 3, 4, 5, 6, 7, 8), user_embedding = TRUE, item_table = c(1, 2, 3, 4, 5, 6, 7, 8))
}
