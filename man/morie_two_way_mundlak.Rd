% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_two_way_mundlak}
\alias{morie_two_way_mundlak}
\title{Pooled OLS with unit-specific and period-specific averages}
\usage{
morie_two_way_mundlak(Y, unit, period, X) 
}
\description{
Sec. 3: adding the unit-specific time averages \eqn{\bar X_i}{Xbar_i} and the period-specific cross-sectional averages \eqn{\bar X_t}{Xbar_t} to a pooled OLS regression reproduces the two-way fixed effects estimates **exactly**.  The design is far smaller: three blocks of width \eqn{p}{p} rather than \eqn{N + T}{N+T} dummies.
}
\arguments{
\item{Y}{Argument \code{Y}; see Usage.}
\item{unit}{Argument \code{unit}; see Usage.}
\item{period}{Argument \code{period}; see Usage.}
\item{X}{Argument \code{X}; see Usage.}
}
\value{
A list with \code{coef} (no intercept), \code{full}   (intercept, X, unit-avg, period-avg), \code{n_columns},   \code{method}, \code{identical_to}.
}
\keyword{internal}
\examples{
morie_two_way_mundlak(Y = c(1, 2, 3, 4, 5, 6, 7, 8), unit = c(1, 2, 3, 4, 5, 6, 7, 8),
  period = c(1, 2, 3, 4, 5, 6, 7, 8), X = c(1, 2, 3, 4, 5, 6, 7, 8))
}
