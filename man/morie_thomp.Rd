% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_thomp}
\alias{morie_thomp}
\title{morie_thomp}
\usage{
morie_thomp(p, T, alpha0 = NULL, beta0 = NULL, seed = 0)
}
\arguments{
\item{p}{See Usage.}
\item{T}{See Usage.}
\item{alpha0}{Defaults to \code{NULL}.}
\item{beta0}{Defaults to \code{NULL}.}
\item{seed}{Defaults to \code{0}.}
}
\description{
Part of the thomp_native implementation; see the file header for the source it follows.
}
\value{
A list with \code{estimate}, \code{actions}, \code{rewards}, \code{alpha}, \code{beta}, \code{post_mean}, \code{counts}, \code{total_reward}, \code{method}.
}
\keyword{internal}
\examples{
morie_thomp(p = 0.5, T = 5L)
}
