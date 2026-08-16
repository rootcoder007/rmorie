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
\item{p}{A vector; its length is taken and its elements indexed.}
\item{T}{A count; the body uses it as \code{seq_len(...)}.}
\item{alpha0}{Defaults to \code{NULL}.}
\item{beta0}{Defaults to \code{NULL}.}
\item{seed}{Passed to \code{.ghc_rng}. Defaults to \code{0}.}
}
\description{
A step of the thomp_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{estimate}, \code{actions}, \code{rewards}, \code{alpha}, \code{beta}, \code{post_mean}, \code{counts}, \code{total_reward}, \code{method}.
}
\keyword{internal}
\examples{
morie_thomp(p = 0.5, T = 5L)
}
