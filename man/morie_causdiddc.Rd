% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_causdiddc}
\alias{morie_causdiddc}
\title{de Chaisemartin-D\'Haultfoeuille (2020) TWFE vs DID_M}
\usage{
morie_causdiddc(Y, D, group, period)
}
\arguments{
\item{Y}{Outcome, long format.}
\item{D}{Binary 0/1 treatment, long format.  Treated status must   be constant within every (group, period) cell.}
\item{group,period}{Panel identifiers.}
}
\description{
The two-way fixed effects coefficient \code{beta_fe} and the paper\'s \code{DID_M} alternative, side by side, plus the Theorem 1 weights that show the gap.  \code{beta_fe} can take the wrong sign under heterogeneity; \code{DID_M} is unbiased for the average effect among switchers under common trends.
}
\value{
A list mirroring the Python RichResult payload:   \code{estimate} (DID_M), \code{beta_fe}, \code{did_m},   \code{weights}, \code{n_negative}, \code{negative_mass},   \code{weight_sum}, \code{n_switches}, \code{gap}, \code{n},   \code{method}, \code{note}.
}
\keyword{internal}
