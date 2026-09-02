% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{greedy_action}
\alias{greedy_action}
\title{Greedy action from per-action next-state distributions}
\usage{
greedy_action(next_probs_by_action, z) 
}
\arguments{
\item{next_probs_by_action}{List of numeric vectors, one per action.}
\item{z}{Numeric vector of atom positions.}
}
\description{
\code{a* = argmax_a sum_i z_i p_i(x\', a)}.
}
\value{
A list with \code{action} (1-based R index) and \code{q_values}   (the per-action means).
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
greedy_action(next_probs_by_action = c(1, 2, 3, 4, 5, 6, 7, 8), z = 5L)
}
