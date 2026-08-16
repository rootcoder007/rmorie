% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_wsm_norm_inv}
\alias{.morie_wsm_norm_inv}
\title{Acklam\\'s rational approximation of the standard normal quantile}
\usage{
.morie_wsm_norm_inv(u)
}
\arguments{
\item{u}{Iterated over elementwise, with \code{vapply}.}
}
\description{
(|error| < 1.15e-9). The PYTHON side uses this too: stats::qnorm is more accurate, but parity requires the same approximation in both languages, so LCG-driven chains agree draw for draw.
}
\value{
A vector, from \code{vapply}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.morie_wsm_norm_inv(V)
}
