% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.tmlmpi_cdf_bank}
\alias{.tmlmpi_cdf_bank}
\title{SPDX-License-Identifier: AGPL-3.0-or-later}
\usage{
.tmlmpi_cdf_bank(yv, Dv, W, g, grid)
}
\arguments{
\item{yv}{See Usage.}
\item{Dv}{See Usage.}
\item{W}{See Usage.}
\item{g}{See Usage.}
\item{grid}{See Usage.}
}
\description{
# Targeted counterfactual CDFs and their influence curves on a grid. # One point-treatment TMLE per threshold on the binary outcome # I(Y <= t), with an arm-specific clever covariate H_a = I(D = a)/g_a so # that each arm mean -- not just the contrast -- solves its own # efficient score. Monotonicity is restored by a running maximum, the # cheapest rearrangement that cannot move a correctly ordered pair.
}
\value{
A list with \code{F}, \code{IC}.
}
\keyword{internal}
