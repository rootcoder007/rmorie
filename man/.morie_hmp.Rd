% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_hmp}
\alias{.morie_hmp}
\title{.morie_hmp}
\usage{
.morie_hmp(p, L = length(p))
}
\arguments{
\item{p}{Numeric; combined arithmetically in the body.}
\item{L}{Numeric; passed to \code{log}. Defaults to \code{length(p)}.}
}
\description{
Asymptotically exact harmonic mean p-value (Wilson 2019, PNAS). The statistic t = mean(1/p) is asymptotically Landau distributed with location log(L) + 0.874367040387922 and scale pi/2; the combined p-value is the Landau upper tail at t. The Landau density has no closed form; integrate its standard integral representation.
}
\value{
A numeric value.
}
\keyword{internal}
