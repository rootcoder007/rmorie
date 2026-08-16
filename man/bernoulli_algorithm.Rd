% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{bernoulli_algorithm}
\alias{bernoulli_algorithm}
\title{Bernoulli (N = 2) alternative}
\usage{
bernoulli_algorithm(reward, gamma, next_probs, v_min, v_max, done = FALSE)
}
\arguments{
\item{reward}{Sampled reward r.}
\item{gamma}{Discount in [0, 1]. Ignored when \code{done = TRUE}.}
\item{next_probs}{Two-element probability vector.}
\item{v_min}{Lower bound of the support.}
\item{v_max}{Upper bound of the support.}
\item{done}{Terminal flag.}
}
\description{
The single-parameter form the C51 paper names in its discussion of the projected operator: \code{Phi T_hat Z = clip((E[T_hat Z] - v_min) / dz, 0, 1)}.
}
\value{
Scalar in [0, 1].
}
\references{
----------
Bellemare, M. G., Dabney, W. & Munos, R. (2017) "A Distributional
Perspective on Reinforcement Learning", ICML 2017 (arXiv:1707.06887)
eq. (7) and Algorithm 1. Rowland, M., Bellemare, M. G., Dabney, W.,
Munos, R. & Teh, Y. W. (2018) "An Analysis of Categorical
Distributional Reinforcement Learning", AISTATS 2018
(arXiv:1802.08163) contraction in Cramer distance.
}
\keyword{internal}
