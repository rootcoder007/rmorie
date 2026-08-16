% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.distq_check_prob}
\alias{.distq_check_prob}
\title{Coerce next_probs to numeric, validate length / sign / sum-to-one}
\usage{
.distq_check_prob(next_probs, n_atoms, name)
}
\arguments{
\item{next_probs}{See Usage.}
\item{n_atoms}{See Usage.}
\item{name}{See Usage.}
}
\description{
Part of the distq_native implementation; see the file header for the source it follows.
}
\value{
The value of \code{p}, as built in the body.
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
