% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.bprMF_recommend}
\alias{.bprMF_recommend}
\title{Rank items for one user by x_hat_ui}
\usage{
.bprMF_recommend(W, H, u, n_items, top_k = 5L, exclude = integer(0))
}
\arguments{
\item{W}{Passed to \code{.bprMF_predict}.}
\item{H}{Passed to \code{.bprMF_predict}.}
\item{u}{See Usage.}
\item{n_items}{A count; the body uses it as \code{seq_len(...)}.}
\item{top_k}{Numeric; passed to \code{min}. Defaults to \code{5L}.}
\item{exclude}{Defaults to \code{integer(0)}.}
}
\description{
A step of the bprMF_native implementation. Called by \code{bpr_recommend_R}. See the file header for the source the module follows.
}
\value{
A list with \code{ranking}, \code{n_scored}.
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
