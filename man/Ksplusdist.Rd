% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{Ksplusdist}
\alias{Ksplusdist}
\title{Exact P(D+ >= c) -- Theorem 4.3.4, Birnbaum-Tingey, p. 115}
\usage{
Ksplusdist(c, n)
}
\description{
Exact P(D+ >= c) -- Theorem 4.3.4, Birnbaum-Tingey, p. 115
}
\value{
A list with \code{sf}, \code{cdf}, \code{terms}, \code{n}, \code{c}.
}
\keyword{internal}
\examples{
Ksplusdist(c = 0.2, n = 20)
}
