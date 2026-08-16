% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{c51_update}
\alias{c51_update}
\title{Algorithm 1 end to end: greedy action, projection, loss}
\usage{
c51_update(reward, gamma, next_probs_by_action, current_probs, v_min, v_max, done = FALSE)
}
\arguments{
\item{reward}{Sampled reward r.}
\item{gamma}{Discount in [0, 1]. Ignored when \code{done = TRUE}.}
\item{next_probs_by_action}{List of per-action next-state   distributions.}
\item{current_probs}{Numeric vector: the current Q-network   distribution for the action under training.}
\item{v_min}{Lower bound of the support.}
\item{v_max}{Upper bound of the support.}
\item{done}{Terminal flag.}
}
\description{
Returns a RichResult-style named list with the projected target \code{m}, the cross-entropy loss, the chosen action, the Q values it was chosen by, and the Q values for the current and target distributions.
}
\value{
Named list with \code{estimate}, \code{loss}, \code{target},   \code{action}, \code{q_values}, \code{q_target}, \code{q_current},   \code{atoms}, \code{n_atoms}, \code{method}.
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
r <- c51_update(reward = 1, gamma = 0.9,
                next_probs_by_action = list(rep(0.2, 5),
                                            c(0.1, 0.1, 0.2, 0.3, 0.3)),
                current_probs = rep(0.2, 5),
                v_min = -10, v_max = 10)
str(r, max.level = 1)
}
