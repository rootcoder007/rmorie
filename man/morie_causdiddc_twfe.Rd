% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_causdiddc_twfe}
\alias{morie_causdiddc_twfe}
\title{The two-way fixed effects coefficient}
\usage{
morie_causdiddc_twfe(Y, D, group, period)
}
\description{
The TWFE coefficient of de Chaisemartin & D\'Haultfoeuille (2020) Theorem 1 and its decomposition: the cell weights, the count and mass of the negative weights, and the treated-cell count.
}
\value{
A list with \code{estimate}, \code{beta_fe}, \code{weights},   \code{n_negative}, \code{negative_mass}, \code{weight_sum},   \code{n_treated_cells}, \code{n}, \code{method}, \code{note}.
}
\keyword{internal}
