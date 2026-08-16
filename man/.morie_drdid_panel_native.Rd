% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_drdid_panel_native}
\alias{.morie_drdid_panel_native}
\title{Internal helper: Sant\'Anna-Zhao doubly robust DiD, panel version}
\usage{
.morie_drdid_panel_native(dy, D, X)
}
\description{
`dy` is the outcome change, `D` the treatment dummy, `X` the intercept-prepended covariate matrix. Returns the ATT and its influence function (the same estimand and IF as `DRDID::drdid_panel`).
}
\value{
A list with \code{att}, \code{IF}, \code{se}.
}
\keyword{internal}
