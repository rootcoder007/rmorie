% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_sdid_native}
\alias{.morie_sdid_native}
\title{Internal helper: native SDID estimator (Algorithm 1, AER 2021)}
\usage{
.morie_sdid_native(Y, N_co, T_pre)
}
\description{
`Y` is the units x periods outcome matrix with the N_co control rows first and N_tr treated rows last; `T_pre` pre-periods come first. Unit weights solve a zeta-regularized simplex LS matching pre-period trajectories (plus a free intercept); time weights match pre-period to mean post-period levels among controls.
}
\value{
A list with \code{estimate}, \code{unit_weights}, \code{time_weights}, \code{zeta}, \code{N_tr}, \code{T_pre}.
}
\keyword{internal}
