% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_sdid_prepare}
\alias{.morie_sdid_prepare}
\title{Internal helper: assemble the SDID input matrix from a long panel}
\usage{
.morie_sdid_prepare(df, outcome, unit, time, treat01)
}
\description{
Returns Y with control rows first, treated rows last, and the pre-period count, given a 0/1 treatment indicator that switches on at a common adoption period.
}
\value{
A list with \code{Y}, \code{N_co}, \code{T_pre}, \code{treated_units}, \code{control_units}.
}
\keyword{internal}
