% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{BWander}
\alias{BWander}
\title{Baseline-wander removal as a transfer function}
\usage{
BWander(z, T = 1, pole = 0.995)
}
\arguments{
\item{z}{A point or vector of points in the z plane; zero is a pole of   any causal transfer function and is refused.}
\item{T}{The sampling interval, which must be positive.}
\item{pole}{The pole radius, at least zero and strictly below one.}
}
\description{
Equation (3.132): the first difference with a pole placed just inside the unit circle at direct current. The pole nearly cancels the zero away from direct current, so the baseline goes without the wholesale high-frequency boost that the bare first difference of equation (3.123) inflicts. A pole exactly at one would cancel the zero outright and is refused.
}
\value{
A list with the transfer function \code{H} at the requested   points and the parameters it was built from.
}
\keyword{internal}
