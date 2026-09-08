% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{distribution_mean}
\alias{distribution_mean}
\title{E[Z] = sum_i z_i p_i}
\usage{
distribution_mean(probs, z) 
}
\arguments{
\item{probs}{Numeric vector of probabilities.}
\item{z}{Numeric vector of atom positions.}
}
\description{
E[Z] = sum_i z_i p_i
}
\value{
Scalar mean of the categorical distribution.
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
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
distribution_mean(V, V)
}
