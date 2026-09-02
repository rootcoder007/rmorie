% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{atoms}
\alias{atoms}
\alias{morie_distq}
\title{The fixed support \{z_i\} and its spacing}
\usage{
atoms(v_min, v_max, n_atoms) 

morie_distq(v_min, v_max, n_atoms) 
}
\arguments{
\item{v_min}{Lower bound of the support.}
\item{v_max}{Upper bound of the support.}
\item{n_atoms}{Number of atoms N. Must be at least 2.}
}
\description{
Atoms on \code{[v_min, v_max]} at equal intervals: \code{z_i = v_min + (i-1) * dz}, \code{dz = (v_max - v_min) / (N - 1)}.
}
\value{
A list with \code{z} (the atom positions) and \code{dz}   (the spacing).
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
atoms(v_min = -10, v_max = 10, n_atoms = 5)
}
