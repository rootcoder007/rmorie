% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_causdiddc_did_m}
\alias{morie_causdiddc_did_m}
\title{The DID_M estimator}
\usage{
morie_causdiddc_did_m(Y, D, group, period)
}
\description{
The \code{DID_M} estimator of de Chaisemartin & D\'Haultfoeuille (2020): for each pair of consecutive periods, compare the outcome change of cells that SWITCH treatment against the change of cells whose treatment stays put, then average over switches, weighting by the number of switching observations.  Unbiased for the average effect among switchers under common trends whatever the heterogeneity.
}
\value{
A list with \code{estimate}, \code{did_m}, \code{switches},   \code{n_switches}, \code{n_switching_obs}, \code{n},   \code{method}, \code{note}.
}
\keyword{internal}
