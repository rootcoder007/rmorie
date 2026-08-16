% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_twoT_retrieve}
\alias{morie_twoT_retrieve}
\title{morie_twoT_retrieve}
\usage{
morie_twoT_retrieve(query_embedding, item_embeddings, probabilities = NULL, top_k = 5, temperature = 1)
}
\arguments{
\item{query_embedding}{Passed to \code{.twoT_as_vec}.}
\item{item_embeddings}{Passed to \code{.twoT_as_mat}.}
\item{probabilities}{Optional; may be \code{NULL}. Passed to \code{.twoT_as_vec}.}
\item{top_k}{Defaults to \code{5}.}
\item{temperature}{Passed to \code{morie_twoT_corrected_logits}. Defaults to \code{1}.}
}
\description{
A step of the twoT_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{estimate}, \code{top_k}, \code{uncorrected_top_k}, \code{scores}, \code{corrected_scores}, \code{changed}, \code{method}, \code{note}.
}
\references{
----------
Yi, X., Yang, J., Hong, L., Cheng, D. Z., Heldt, L., Kumthekar, A.,
Zhao, Z., Wei, L. & Chi, E. (2019) "Sampling-Bias-Corrected Neural
Modeling for Large Corpus Item Recommendations", *Proceedings of the
13th ACM Conference on Recommender Systems (RecSys \'19)*, 269-277,
doi:10.1145/3298689.3346996. The two-tower architecture for
large-corpus item retrieval; batch softmax with in-batch negatives and
the resulting sampling bias toward popular items; the logQ correction
subtracting log p_j from the logit; the streaming frequency estimation
algorithm tracking the number of steps between successive hits of an
item and estimating the sampling probability as its reciprocal; and
normalisation of the embeddings with a temperature in the softmax.

Bengio, Y. & Senecal, J.-S. (2008) "Adaptive Importance Sampling to
Accelerate Training of a Neural Probabilistic Language Model", *IEEE
Transactions on Neural Networks* 19(4), 713-722,
doi:10.1109/TNN.2007.912312. The sampled-softmax correction being
applied.

Covington, P., Adams, J. & Sargin, E. (2016) "Deep Neural Networks
for YouTube Recommendations", *RecSys 2016*, 191-198,
doi:10.1145/2959100.2959190. The retrieval setting.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
morie_twoT_retrieve(V, V)
}
