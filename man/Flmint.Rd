% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{Flmint}
\alias{Flmint}
\title{Functional linear model with scalar response (MVSML eq. 14.1)}
\usage{
Flmint(t, x_values, beta_values, mu = 0)
}
\description{
Y = mu + int_0^T x(t) beta(t) dt + E.  The integral of the product of the centered covariate curve and the coefficient function is taken by the trapezoid rule on the observation grid, the same quadrature the chapter uses for its inner products on p.581.
}
\value{
A list with \code{integral}, \code{fitted}, \code{mu}, \code{n_points}.
}
\keyword{internal}
