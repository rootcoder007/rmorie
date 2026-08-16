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
\item{alpha}{See Usage.}
\item{K}{See Usage.}
\item{rng}{Defaults to \code{NULL}.}
\item{seed}{Defaults to \code{0}.}
}
\description{
Part of the slowdp_native implementation; see the file header for the source it follows.
}
\value{
A list with \code{weights}, \code{V}, \code{remaining}, \code{kept_mass}, \code{K}, \code{alpha}, \code{note}.
}
\keyword{internal}
\examples{
stick_breaking(alpha = 0.5, K = 5L)
}
