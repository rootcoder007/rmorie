% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_kamath_rtd_loss}
\alias{morie_kamath_rtd_loss}
\title{morie_kamath_rtd_loss}
\usage{
morie_kamath_rtd_loss(xhat, d) 
}
\arguments{
\item{xhat}{Per-token probability of ORIGINAL.}
\item{d}{Labels.}
}
\description{
morie_kamath_rtd_loss
}
\value{
A list with \code{estimate}, \code{per_token}, \code{accuracy}, \code{n}, \code{method}.
}
\keyword{internal}
\examples{
morie_kamath_rtd_loss(c(0.9, 0.9), c(1, 0))
}
