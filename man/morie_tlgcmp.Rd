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
\item{strata}{Passed to \code{g_computation}.}
\item{outcome_means}{Passed to \code{g_computation}.}
\item{covariate_probs}{Passed to \code{g_computation}.}
\item{Q_functions}{Passed to \code{sequential_g_formula}.}
\item{L_supports}{Passed to \code{sequential_g_formula}.}
\item{L_probs}{Passed to \code{sequential_g_formula}.}
\item{rule}{Passed to \code{sequential_g_formula}.}
\item{Y}{Passed to \code{counterfactual_mean}.}
\item{A}{Passed to \code{counterfactual_mean}.}
\item{L}{Passed to \code{counterfactual_mean}.}
\item{a_star}{Passed to \code{counterfactual_mean}.}
\item{g}{Passed to \code{positivity_check}.}
\item{delta}{Passed to \code{positivity_check}. Defaults to \code{0.01}.}
\item{mode}{One of \code{"counterfactual"}, \code{"positivity"}, \code{"sequential"}.}
}
\description{
A step of the tlgcmp_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
The value of \code{g_computation}.
}
\keyword{internal}
