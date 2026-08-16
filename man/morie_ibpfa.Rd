% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_ibpfa}
\alias{morie_ibpfa}
\title{morie_ibpfa}
\usage{
morie_ibpfa(n, alpha, seed = 0L)
}
\arguments{
\item{n}{Passed to \code{sample_ibp}.}
\item{alpha}{Passed to \code{sample_ibp}.}
\item{seed}{Passed to \code{sample_ibp}. Defaults to \code{0L}.}
}
\description{
A step of the ibpfa_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
The value of \code{sample_ibp}.
}
\keyword{internal}
\examples{
morie_ibpfa(n = 5L, alpha = 0.5)
}
