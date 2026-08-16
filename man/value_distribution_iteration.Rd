% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{value_distribution_iteration}
\alias{value_distribution_iteration}
\title{Value-distribution iteration}
\usage{
value_distribution_iteration(reward_atoms, reward_probs, gamma, v_min, v_max, n_atoms, iters = 400L, tol = 1e-13)
}
\arguments{
\item{reward_atoms}{Numeric vector of possible reward values.}
\item{reward_probs}{Numeric vector of reward probabilities (sums   to 1).}
\item{gamma}{Discount in [0, 1).}
\item{v_min}{Lower bound of the support.}
\item{v_max}{Upper bound of the support.}
\item{n_atoms}{Number of atoms.}
\item{iters}{Maximum number of iterations.}
\item{tol}{Convergence tolerance on the max abs shift.}
}
\description{
Iterate the projected operator on a single self-looping state. The reward is resampled from \code{(reward_atoms, reward_probs)} each step. The projected operator is a Cramer-contraction (Rowland et al. 2018), so this converges to the stationary value distribution.
}
\value{
A list with \code{distribution} (the final categorical   distribution) and \code{info} (a named list with   \code{iterations}, \code{converged}, \code{shift}).
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
\examples{
r <- value_distribution_iteration(reward_atoms = c(0, 1),
                                  reward_probs = c(0.5, 0.5),
                                  gamma = 0.9, v_min = 0, v_max = 15,
                                  n_atoms = 21)
str(r, max.level = 1)
}
