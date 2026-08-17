% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{wrap_angle}
\alias{wrap_angle}
\title{Wrap to [-pi, pi)}
\usage{
wrap_angle(theta)
}
\arguments{
\item{theta}{Angle in radians.}
}
\description{
Wrap to [-pi, pi)
}
\value{
Angle in [-pi, pi).
}
\keyword{internal}
\examples{
w <- wrap_angle(2.5 * pi)
abs(w - 0.5 * pi) < 1e-9
}
