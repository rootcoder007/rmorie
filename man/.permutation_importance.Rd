% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.permutation_importance}
\alias{.permutation_importance}
\title{Permutation importance on the SAME forest}
\usage{
.permutation_importance(trees, X, y, features, seed, n_repeats)
}
\arguments{
\item{trees}{A list of trees.}
\item{X}{Numeric covariate matrix.}
\item{y}{Numeric pseudo-outcome.}
\item{features}{Optional integer vector of feature indices.}
\item{seed}{Integer seed.}
\item{n_repeats}{Integer number of shuffles per feature.}
}
\description{
The rise in the weighted prediction error when a covariate is shuffled. Uses the shared generator so the same seed reproduces the same ordering as the Python arm.
}
\value{
A list with \code{importance} and \code{baseline_error}.
}
\keyword{internal}
