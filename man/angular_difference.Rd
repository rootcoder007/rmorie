% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{angular_difference}
\alias{angular_difference}
\title{Signed shortest difference a - b, in [-pi, pi)}
\usage{
angular_difference(a, b)
}
\arguments{
\item{a}{First angle.}
\item{b}{Second angle.}
}
\description{
Signed shortest difference a - b, in [-pi, pi)
}
\value{
Wrapped difference in [-pi, pi).
}
\keyword{internal}
\examples{
d <- angular_difference(0.1, 2 * pi + 0.4)
abs(d - (-0.3)) < 1e-9
}
