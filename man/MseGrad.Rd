% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{MseGrad}
\alias{MseGrad}
\title{Eq. (3.167): grad J = -2 Theta + 2 Phi w.  The surface is quadratic}
\usage{
MseGrad(phi, theta, w)
}
\arguments{
\item{phi}{See Usage.}
\item{theta}{See Usage.}
\item{w}{See Usage.}
}
\description{
with a single minimum, so a vanishing gradient is the optimum and not merely a stationary point.
}
\value{
A list with \code{gradient}, \code{norm}, \code{at_optimum}, \code{order}, \code{surface}, \code{method}.
}
\keyword{internal}
\examples{
MseGrad(phi = 0.5, theta = 0.5, w = 5L)
}
