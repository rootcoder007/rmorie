% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.distq_atoms_full}
\alias{.distq_atoms_full}
\title{(Internal companion returning both z and dz -- used by the other}
\usage{
.distq_atoms_full(v_min, v_max, n_atoms)
}
\arguments{
\item{v_min}{Passed to \code{.distq_atoms}.}
\item{v_max}{Passed to \code{.distq_atoms}.}
\item{n_atoms}{Passed to \code{.distq_atoms}.}
}
\description{
routines so we don\\'t rebuild the spacing each time.)
}
\value{
The value of \code{.distq_atoms}.
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
