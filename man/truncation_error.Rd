% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{truncation_error}
\alias{truncation_error}
\title{truncation_error}
\usage{
truncation_error(alpha, K)
}
\arguments{
\item{alpha}{See Usage.}
\item{K}{See Usage.}
}
\description{
A step of the slowdp_native implementation. Called by \code{decay_diagnostics}, \code{sticks_for_tolerance}, \code{truncated_dp}. See the file header for the source the module follows.
}
\value{
A list with \code{expected_tail}, \code{kept}, \code{alpha}, \code{K}, \code{per_stick_factor}, \code{note}.
}
\keyword{internal}
\examples{
truncation_error(alpha = 0.5, K = 5L)
}
