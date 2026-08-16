% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_timesfm_horizon_plan}
\alias{morie_timesfm_horizon_plan}
\title{morie_timesfm_horizon_plan}
\usage{
morie_timesfm_horizon_plan(horizon, input_patch_len, output_patch_len)
}
\arguments{
\item{horizon}{Coerced to integer by the body, with \code{as.integer}.}
\item{input_patch_len}{Coerced to integer by the body, with \code{as.integer}.}
\item{output_patch_len}{Coerced to integer by the body, with \code{as.integer}.}
}
\description{
A step of the timesfm_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{steps_asymmetric}, \code{steps_symmetric}, \code{steps_direct}, \code{input_patch_len}, \code{output_patch_len}, \code{horizon}, \code{speedup_vs_symmetric}, \code{note}.
}
\keyword{internal}
