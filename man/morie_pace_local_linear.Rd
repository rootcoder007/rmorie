% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_pace_local_linear}
\alias{morie_pace_local_linear}
\title{Local linear smoother on a one-dimensional grid}
\usage{
morie_pace_local_linear(t, y, at, bw, kernel = "epan")
}
\arguments{
\item{t,y}{The observation times and values, pooled over subjects.}
\item{at}{Points to evaluate at.}
\item{bw}{Bandwidth.}
\item{kernel}{\code{"epan"} or \code{"gauss"}.}
}
\description{
Weighted least squares of \code{y} on \code{t - t0} at each point of \code{at}, taking the intercept. Where the design is degenerate -- one distinct point inside the bandwidth -- the fit falls back to the weighted mean, which is the local CONSTANT fit and the only thing identified there.
}
\value{
A numeric vector, one fitted value per point of \code{at}.
}
\keyword{internal}
\examples{
set.seed(1)
t <- sort(runif(80))
y <- sin(2 * pi * t) + rnorm(80, 0, 0.1)
at <- seq(0.05, 0.95, length.out = 11)
fit <- morie_pace_local_linear(t, y, at, bw = 0.12)
c(length(fit), max(abs(fit - sin(2 * pi * at))) < 0.3)
}
