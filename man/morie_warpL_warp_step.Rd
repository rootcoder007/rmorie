% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_warpL_warp_step}
\alias{morie_warpL_warp_step}
\title{morie_warpL_warp_step}
\usage{
morie_warpL_warp_step(positive, negatives, embed_user, rng, alphas, lr = 0.05, margin = 1) 
}
\arguments{
\item{positive}{Passed to \code{.warpL_vec}.}
\item{negatives}{A vector; its length is taken and its elements indexed.}
\item{embed_user}{Passed to \code{.warpL_vec}.}
\item{rng}{Passed to \code{.warpL_sample_violation}.}
\item{alphas}{Passed to \code{.warpL_warp_loss}.}
\item{lr}{Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{0.05}.}
\item{margin}{Passed to \code{.warpL_sample_violation}. Defaults to \code{1}.}
}
\description{
A step of the warpL_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{estimate}, \code{updated}, \code{loss}, \code{user}, \code{draws}, \code{estimated_rank}, \code{rank_weight}, \code{negative}, \code{method}, \code{note}.
}
\references{
----------
Weston, J., Bengio, S. & Usunier, N. (2010) "Large Scale Image
Annotation: Learning to Rank with Joint Word-Image Embeddings",
Machine Learning and Knowledge Discovery in Databases (ECML PKDD
2010), LNCS 6323, 21-35, doi:10.1007/978-3-642-15939-8_2. [PDF
supplied by Vee.] The WARP (Weighted Approximate-Rank Pairwise) loss:
that measures optimising for the top annotations, such as precision at
k, are costly to train; the relation to the Ordered Weighted Pairwise
Classification loss; the use of stochastic gradient descent with a
sampling trick to APPROXIMATE ranks, giving an efficient online
strategy superior to standard SGD on the same loss and able to train
on datasets that do not fit in memory; and its applicability to
arbitrary differentiable models, unlike the OWPC loss which relies on
SVMstruct.

Usunier, N., Buffoni, D. & Gallinari, P. (2009) "Ranking with ordered
weighted pairwise classification", ICML 2009, 1057-1064,
doi:10.1145/1553374.1553509. The ordered weighted pairwise loss and
the alpha weights.

Weston, J., Bengio, S. & Usunier, N. (2011) "WSABIE: Scaling Up to
Large Vocabulary Image Annotation", IJCAI 2011, 2764-2770. The
later, more widely cited presentation of the same loss. NOTE: the
ECML 2010 paper above is the one held locally and is the text this
module follows.
}
\keyword{internal}
