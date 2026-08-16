% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{poltrx_tree_density}
\alias{poltrx_tree_density}
\title{Piecewise-constant density implied by a finite Polya tree}
\usage{
poltrx_tree_density(tree, level = NULL, lo = 0, hi = 1)
}
\arguments{
\item{tree}{A tree as returned by \code{poltrx_finite_tree}.}
\item{level}{Optional override of the level; defaults to the   tree\'s truncation level.}
\item{lo}{Lower bound of the display interval.}
\item{hi}{Upper bound of the display interval.}
}
\description{
The probabilities at a level sum to 1 by construction, whatever the branch draws were.
}
\value{
A list with \code{estimate}, \code{density},   \code{probabilities}, \code{edges}, \code{level}, \code{total},   \code{method} and \code{note}.
}
\keyword{internal}
