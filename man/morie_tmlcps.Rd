% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_tmlcps}
\alias{morie_tmlcps}
\title{The effect curve theta(a) = E(Y^a) for a continuous treatment}
\usage{
morie_tmlcps(y, A, X, a_grid = NULL, fit = "kernel", bandwidth = NULL, n_folds = 5)
}
\arguments{
\item{y}{Outcome vector.}
\item{A}{Continuous treatment vector.}
\item{X}{Covariate matrix.}
\item{a_grid}{Grid of treatment values.}
\item{fit}{Stage-2 regression.}
\item{bandwidth}{Optional bandwidth.}
\item{n_folds}{Number of CV folds.}
}
\description{
The effect curve theta(a) = E(Y^a) for a continuous treatment
}
\value{
A list with \code{estimate}, \code{se}, \code{curve},   \code{grid}, \code{slopes}, \code{pseudo_outcome},   \code{bandwidth}, \code{marginal_density}, \code{standardized_mu},   \code{pi_obs}, \code{fit}, \code{n}, \code{method}.
}
\keyword{internal}
