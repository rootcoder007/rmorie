% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.smd_moments}
\alias{.smd_moments}
\title{Internal: weighted mean and the weighted variance used for the SMD}
\usage{
.smd_moments(x, w)
}
\arguments{
\item{x}{Numeric; combined arithmetically in the body.}
\item{w}{Numeric; passed to \code{sum}.}
}
\description{
The sw - sum(w^2)/sw denominator reduces to n - 1 at equal weights, so the weighted and unweighted diagnostics agree there.
}
\value{
A vector, from \code{c}.
}
\keyword{internal}
