% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{sample_ibp}
\alias{sample_ibp}
\title{sample_ibp}
\usage{
sample_ibp(n, alpha, seed = 0L)
}
\arguments{
\item{n}{Coerced to integer by the body, with \code{as.integer}.}
\item{alpha}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{seed}{Passed to \code{.ghc_rng}. Defaults to \code{0L}.}
}
\description{
A step of the ibpfa_native implementation. Called by \code{morie_ibpfa}. See the file header for the source the module follows.
}
\value{
A list with \code{Z}, \code{K}, \code{counts}, \code{alpha}, \code{n}, \code{features_per_object}, \code{note}.
}
\keyword{internal}
\examples{
sample_ibp(n = 5L, alpha = 0.5)
}
