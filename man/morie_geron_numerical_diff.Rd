% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_geron_numerical_diff}
\alias{morie_geron_numerical_diff}
\title{Numerical differentiation via central finite differences, Richardson-checked (Geron App A, hmnmd)}
\usage{
morie_geron_numerical_diff(f, x, h = 1e-05)
}
\arguments{
\item{f}{Function `f(x) -> scalar`.}
\item{x}{Point (scalar or vector).}
\item{h}{Step size.}
}
\description{
Numerical differentiation via central finite differences, Richardson-checked (Geron App A, hmnmd)
}
\value{
A list with \code{derivative}, \code{richardson}, \code{error_estimate}, \code{n_evals}, \code{h}, \code{estimate}, \code{n}, \code{method}.
}
\keyword{internal}
\examples{
x <- matrix(c(1, 0, 0, 1, 1, 1), ncol = 2, byrow = TRUE)
morie_geron_numerical_diff(function(x) x^3, 2)
}
