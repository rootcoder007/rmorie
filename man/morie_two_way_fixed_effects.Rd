% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_two_way_fixed_effects}
\alias{morie_two_way_fixed_effects}
\title{Pooled OLS with a full set of unit and period dummies}
\usage{
morie_two_way_fixed_effects(Y, unit, period, X)
}
\arguments{
\item{Y}{N-vector of outcomes.}
\item{unit,period}{N-vectors of panel identifiers.}
\item{X}{N-by-p matrix of covariates.}
}
\description{
The reference cell is dropped from each set so the design has full rank.  Returns the coefficients on \code{X} only -- the fixed effects are nuisance.
}
\value{
A list with \code{coef} (length-p numeric, no intercept),   \code{full} (numeric vector of all fitted coefficients   including the intercept, in order: intercept, X, unit dummies,   period dummies), \code{n_units}, \code{n_periods},   \code{n_columns}, \code{method}.
}
\keyword{internal}
\examples{
morie_two_way_fixed_effects(Y = c(1, 2, 3, 4, 5, 6, 7, 8), unit = c(1, 2, 3, 4, 5, 6, 7, 8), period = c(1, 2, 3, 4, 5, 6, 7, 8), X = c(1, 2, 3, 4, 5, 6, 7, 8))
}
