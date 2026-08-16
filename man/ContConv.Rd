% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{ContConv}
\alias{ContConv}
\title{Eq (3.30): y(t) = integral x(tau) h(t - tau) d tau.  Tabulated on a}
\usage{
ContConv(x, h, dt = 1, t = NULL)
}
\arguments{
\item{x}{See Usage.}
\item{h}{See Usage.}
\item{dt}{Defaults to \code{1}.}
\item{t}{Defaults to \code{NULL}.}
}
\description{
uniform grid this is the discrete convolution SCALED BY dt; dropping the dt is how a continuous-time convolution comes out wrong by a factor of the sampling interval.
}
\value{
A list with \code{y}, \code{t}, \code{dt}, \code{n}, \code{m}, \code{integral}, \code{method}.
}
\keyword{internal}
