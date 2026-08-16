% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{bpr_recommend_R}
\alias{bpr_recommend_R}
\title{Recommend top items for one user by x_hat_ui}
\usage{
bpr_recommend_R(W, H, u, n_items, top_k = 5L, exclude = integer(0))
}
\arguments{
\item{top_k}{Number of items to return.}
\item{exclude}{Integer vector of 0-based item indices to drop.}
}
\description{
Mirrors morie.fn.bprMF.recommend.
}
\value{
List with \code{ranking} (list of \code{list(i=, s=)} up to   \code{top_k}) and \code{n_scored}.
}
\references{
----------
Rendle, S., Freudenthaler, C., Gantner, Z. & Schmidt-Thieme, L.
(2009) "BPR: Bayesian Personalized Ranking from Implicit Feedback",
Proceedings of the Twenty-Fifth Conference on Uncertainty in
Artificial Intelligence (UAI 2009), 452-461, arXiv:1205.2618.
Sec. 3 (the objection to labelling all unobserved pairs negative,
and the construction of D_S). Sec. 4.1 (the likelihood
sigma(x_uij), the Gaussian prior, and BPR-Opt). Sec. 4.1.1 (the AUC
analogy: identical but for the Heaviside vs ln sigma loss).
Sec. 4.2 and Figure 4 (LearnBPR by bootstrap sampling stochastic
gradient descent). Sec. 4.3 (matrix factorisation, x_uij = x_ui -
x_uj, and the three derivative cases).

Koren, Y., Bell, R. & Volinsky, C. (2009) "Matrix Factorization
Techniques for Recommender Systems", Computer 42(8), 30-37,
doi:10.1109/MC.2009.263. The factorisation model class BPR is
applied to here.
"""

Bit-identical R mirror of src/morie/fn/bprMF.py. All randomness comes
from the shared SplitMix64 stream (.ghc_rng / .ghc_unif), consumed
draw for draw in the same order: U*K uniforms to fill W, I*K to fill
H, then per LearnBPR iteration three uniforms (user index, positive
item, negative item) plus up to 100 more for the negative-item
rejection guard, exactly as the Python arm does.
}
\keyword{internal}
\examples{
bpr_recommend_R(W = c(1, 2, 3, 4, 5, 6, 7, 8), H = 0.5, u = 5L, n_items = c(1, 2, 3, 4, 5, 6, 7, 8))
}
