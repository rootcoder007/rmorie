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
\item{alpha}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{K}{Coerced to integer by the body, with \code{as.integer}.}
\item{base_sampler}{Optional; may be \code{NULL}. Passed to \code{is.null}.}
\item{rng}{Optional; may be \code{NULL}. Passed to \code{is.null}.}
\item{seed}{Passed to \code{.ghc_rng}. Defaults to \code{0}.}
\item{renormalise}{A flag; the body branches on it. Defaults to \code{TRUE}.}
}
\description{
A step of the slowdp_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{estimate}, \code{weights}, \code{atoms}, \code{discarded_mass}, \code{expected_discarded}, \code{renormalised}, \code{K}, \code{alpha}, \code{method}, \code{note}.
}
\keyword{internal}
\examples{
truncated_dp(alpha = 0.5, K = 5L)
}
