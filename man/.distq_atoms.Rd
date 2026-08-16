% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.distq_atoms}
\alias{.distq_atoms}
\title{Build the atom support z_i = v_min + i * dz, with dz = (v_max -}
\usage{
.distq_atoms(v_min, v_max, n_atoms)
}
\arguments{
\item{v_min}{See Usage.}
\item{v_max}{See Usage.}
\item{n_atoms}{See Usage.}
}
\description{
v_min) / (n_atoms - 1), and the spacing. n must be at least 2 and v_max > v_min.
}
\value{
A list with \code{z}, \code{dz}.
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
