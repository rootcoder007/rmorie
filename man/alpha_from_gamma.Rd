% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{alpha_from_gamma}
\alias{alpha_from_gamma}
\title{Invert 1 / (alpha (1 - alpha)) = gamma}
\usage{
alpha_from_gamma(gamma) 
}
\arguments{
\item{gamma}{Threshold.}
}
\description{
Undefined below gamma = 4, where the quadratic has no real root.
}
\value{
alpha in (0, 1/2].
}
\keyword{internal}
\examples{
alpha_from_gamma(gamma = 9)
}
