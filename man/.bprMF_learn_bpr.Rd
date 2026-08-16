% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.bprMF_learn_bpr}
\alias{.bprMF_learn_bpr}
\title{LearnBPR: bootstrap-sampled stochastic gradient ascent over triples}
\usage{
.bprMF_learn_bpr(pos, n_users, n_items, k_dim = 8L, alpha = 0.05, lam = 0.01, iters = 2000L, seed = 0L, regularizer_sign = "correct", init_scale = 0.1)
}
\arguments{
\item{pos}{See Usage.}
\item{n_users}{See Usage.}
\item{n_items}{See Usage.}
\item{k_dim}{Defaults to \code{8L}.}
\item{alpha}{Defaults to \code{0.05}.}
\item{lam}{Defaults to \code{0.01}.}
\item{iters}{Defaults to \code{2000L}.}
\item{seed}{Defaults to \code{0L}.}
\item{regularizer_sign}{Defaults to \code{"correct"}.}
\item{init_scale}{Defaults to \code{0.1}.}
}
\description{
drawn from D_S. Mirrors morie.fn.bprMF.learn_bpr step for step, with the same draw order from the shared SplitMix64 stream: U*K uniforms fill W, I*K uniforms fill H, then per iteration three uniforms (user index, positive item, negative item) plus up to 100 negative-item rejection-guard uniforms.  regularizer_sign="paper" reproduces the printed Figure 4 update whose +lambda*Theta term diverges; the default is the sign that actually ascends BPR-Opt.
}
\value{
A list with \code{estimate}, \code{W}, \code{H}, \code{k}, \code{bpr_opt_history}, \code{final_bpr_opt}, \code{auc}, \code{param_norm}, \code{regularizer_sign}, \code{method}, \code{caveat}.
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
