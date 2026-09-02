% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_pace_local_linear_2d}
\alias{morie_pace_local_linear_2d}
\title{Local linear smoother on a two-dimensional grid}
\usage{
morie_pace_local_linear_2d(s, t, z, at_s, at_t, bw, kernel = "epan") 
}
\arguments{
\item{s,t,z}{The two coordinates and the value at each pair.}
\item{at_s,at_t}{Grids to evaluate on.}
\item{bw}{Bandwidth, shared by both coordinates.}
\item{kernel}{\code{"epan"} or \code{"gauss"}.}
}
\description{
The covariance surface: a plane fitted in \code{(s, t)} around each grid point, taking the intercept. Points are bucketed by bandwidth so each fit touches only the neighbouring cells -- the Epanechnikov kernel vanishes past one bandwidth, the Gaussian is truncated at four, which is the reach the Python arm uses.
}
\value{
A matrix with one row per \code{at_s} and column per   \code{at_t}.
}
\keyword{internal}
\examples{
set.seed(1)
s <- runif(150); t <- runif(150)
z <- sin(2 * s) + cos(2 * t) + rnorm(150, 0, 0.1)
grid <- seq(0.2, 0.8, length.out = 4)
fit <- morie_pace_local_linear_2d(s, t, z, grid, grid, bw = 0.35)
length(fit)
}
