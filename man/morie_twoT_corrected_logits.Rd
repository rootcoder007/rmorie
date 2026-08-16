% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_twoT_corrected_logits}
\alias{morie_twoT_corrected_logits}
\title{morie_twoT_corrected_logits}
\usage{
morie_twoT_corrected_logits(scores, probabilities, temperature = 1)
}
\arguments{
\item{scores}{Passed to \code{.twoT_as_vec}.}
\item{probabilities}{Passed to \code{.twoT_as_vec}.}
\item{temperature}{Defaults to \code{1}.}
}
\description{
A step of the twoT_native implementation. Called by \code{morie_twoT_batch_softmax_loss}, \code{morie_twoT_retrieve}. See the file header for the source the module follows.
}
\value{
A list with \code{corrected}, \code{raw}, \code{shift}, \code{note}.
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
