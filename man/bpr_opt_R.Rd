% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{bpr_opt_R}
\alias{bpr_opt_R}
\title{BPR-Opt and its loglik / penalty pieces}
\usage{
bpr_opt_R(W, H, pos, n_items, lam = 0.01)
}
\arguments{
\item{W}{List of length \code{n_users}; each element a length-k numeric vector.}
\item{H}{List of length \code{n_items}; each element a length-k numeric vector.}
\item{pos}{Named list of positive-item indices per user (0-based).}
\item{n_items}{Number of items.}
\item{lam}{Regularisation strength \eqn{\lambda}.}
}
\description{
\eqn{\sum_{(u,i,j) in D_S} \ln \sigma(\hat x_{uij}) - \lambda \|\Theta\|^2}. Mirrors morie.fn.bprMF.bpr_opt.
}
\value{
A list with \code{bpr_opt}, \code{loglik}, \code{penalty},   \code{n_triples}.
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
