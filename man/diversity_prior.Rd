% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{diversity_prior}
\alias{diversity_prior}
\title{Diversity prior \eqn{\bar p = \beta p_0 + (1-\beta) p(g \mid u)}.}
\usage{
diversity_prior(p_u, p0, beta)
}
\arguments{
\item{p_u}{User\'s played distribution \eqn{p(g \mid u)}.}
\item{p0}{Population prior \eqn{p_0(g)}.}
\item{beta}{Mixing weight in \eqn{[0, 1]}.}
}
\description{
Calibration alone never introduces an unplayed genre; mixing in a diversity prior with weight \code{beta} is the explicit escape hatch from the filter bubble.
}
\value{
Numeric vector \eqn{\bar p(g)} of length G.
}
\keyword{internal}
\examples{
diversity_prior(p_u = c(1, 2, 3, 4, 5, 6, 7, 8), p0 = c(1, 2, 3, 4, 5, 6, 7, 8), beta = 0.5)
}
