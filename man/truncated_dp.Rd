% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{truncated_dp}
\alias{truncated_dp}
\title{truncated_dp}
\usage{
truncated_dp(alpha, K, base_sampler = NULL, rng = NULL, seed = 0, renormalise = TRUE)
}
\arguments{
\item{alpha}{See Usage.}
\item{K}{See Usage.}
\item{base_sampler}{Defaults to \code{NULL}.}
\item{rng}{Defaults to \code{NULL}.}
\item{seed}{Defaults to \code{0}.}
\item{renormalise}{Defaults to \code{TRUE}.}
}
\description{
Part of the slowdp_native implementation; see the file header for the source it follows.
}
\value{
A list with \code{estimate}, \code{weights}, \code{atoms}, \code{discarded_mass}, \code{expected_discarded}, \code{renormalised}, \code{K}, \code{alpha}, \code{method}, \code{note}.
}
\keyword{internal}
\examples{
truncated_dp(alpha = 0.5, K = 5L)
}
