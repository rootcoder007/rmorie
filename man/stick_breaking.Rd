% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{stick_breaking}
\alias{stick_breaking}
\title{stick_breaking}
\usage{
stick_breaking(alpha, K, rng = NULL, seed = 0)
}
\arguments{
\item{alpha}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{K}{Coerced to integer by the body, with \code{as.integer}.}
\item{rng}{Optional; may be \code{NULL}. Passed to \code{is.null}.}
\item{seed}{Passed to \code{.ghc_rng}. Defaults to \code{0}.}
}
\description{
A step of the slowdp_native implementation. Called by \code{truncated_dp}. See the file header for the source the module follows.
}
\value{
A list with \code{weights}, \code{V}, \code{remaining}, \code{kept_mass}, \code{K}, \code{alpha}, \code{note}.
}
\keyword{internal}
\examples{
stick_breaking(alpha = 0.5, K = 5L)
}
