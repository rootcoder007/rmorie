% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_tlgcmp}
\alias{morie_tlgcmp}
\title{morie_tlgcmp}
\usage{
morie_tlgcmp(strata = NULL, outcome_means = NULL, covariate_probs = NULL, Q_functions = NULL, L_supports = NULL, L_probs = NULL, rule = NULL, Y = NULL, A = NULL, L = NULL, a_star = NULL, g = NULL, delta = 0.01, mode = c("point", "sequential", "counterfactual", "positivity"))
}
\arguments{
\item{strata}{Defaults to \code{NULL}.}
\item{outcome_means}{Defaults to \code{NULL}.}
\item{covariate_probs}{Defaults to \code{NULL}.}
\item{Q_functions}{Defaults to \code{NULL}.}
\item{L_supports}{Defaults to \code{NULL}.}
\item{L_probs}{Defaults to \code{NULL}.}
\item{rule}{Defaults to \code{NULL}.}
\item{Y}{Defaults to \code{NULL}.}
\item{A}{Defaults to \code{NULL}.}
\item{L}{Defaults to \code{NULL}.}
\item{a_star}{Defaults to \code{NULL}.}
\item{g}{Defaults to \code{NULL}.}
\item{delta}{Defaults to \code{0.01}.}
\item{mode}{One of \code{"counterfactual"}, \code{"positivity"}, \code{"sequential"}.}
}
\description{
A step of the tlgcmp_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
The value of \code{g_computation}.
}
\keyword{internal}
