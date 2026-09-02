% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{categorical_projection}
\alias{categorical_projection}
\title{Eq. (7) / Algorithm 1: project r + gamma * z onto {z_i}}
\usage{
categorical_projection(reward, gamma, next_probs, v_min, v_max, n_atoms = NULL, done = FALSE) 
}
\arguments{
\item{reward}{Sampled reward r.}
\item{gamma}{Discount in [0, 1]. Ignored when \code{done = TRUE}.}
\item{next_probs}{Numeric vector of length n_atoms: the next-state   distribution under the greedy action. Must be a probability vector.}
\item{v_min}{Lower bound of the support.}
\item{v_max}{Upper bound of the support.}
\item{n_atoms}{Number of atoms. If \code{NULL} (the default), inferred   from \code{next_probs}.}
\item{done}{Terminal flag; equivalent to \code{gamma = 0}.}
}
\description{
For each atom j compute \code{tz = clip(r + gamma * z_j, v_min, v_max)}, its fractional position \code{b = (tz - v_min) / dz}, and split \code{p_j} between the two integer neighbours \code{l = floor(b)} and \code{u = ceil(b)}. The exact-hit case \code{l == u} is fixed: instead of writing \code{p * (u - b) + p * (b - l) = 0}, the full mass is added to \code{m[l]}.
}
\value{
Numeric vector of length n_atoms: the projected target   probabilities, which sum to 1.
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
m <- categorical_projection(reward = 1, gamma = 0.9,
                            next_probs = rep(0.2, 5),
                            v_min = -10, v_max = 10)
stopifnot(abs(sum(m) - 1) < 1e-9)
m
}
