% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_dpgrf}
\alias{morie_dpgrf}
\title{Areal difference boundaries by Dirichlet-process clustering}
\usage{
morie_dpgrf(W, label_draws, threshold = 0.5)
}
\arguments{
\item{W}{Square symmetric adjacency matrix (n x n).}
\item{label_draws}{List of integer label vectors, each of length n.}
\item{threshold}{Probability threshold above which a pair is   declared a boundary.}
}
\description{
Computes the posterior probability that two adjacent regions differ -- \code{P(phi_i != phi_j)} -- from a set of sampled cluster labels, then flags the pairs whose probability exceeds a threshold. The probability is a posterior statement about cluster membership under a Dirichlet-process mixture prior on the areal random effects, NOT a thresholded difference of continuous values, which is the whole point of using a DP rather than a plain CAR.
}
\value{
A list with \code{estimate} (boundary pairs), \code{boundaries}   (same as \code{estimate}), \code{ranked} (list of one entry per   adjacent pair, sorted by descending \code{p_difference}),   \code{n_adjacent}, \code{n_boundaries}, \code{threshold},   \code{method}, \code{note}.
}
\keyword{internal}
\examples{
morie_dpgrf(W = 5L, label_draws = c(1, 2, 3, 4, 5, 6, 7, 8))
}
