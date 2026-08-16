% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_gh_hellinger}
\alias{.morie_gh_hellinger}
\title{.morie_gh_hellinger}
\usage{
.morie_gh_hellinger(p, q, grid)
}
\arguments{
\item{p}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{q}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{grid}{Coerced to numeric by the body, with \code{as.numeric}.}
}
\description{
A step of the ghosal_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A numeric value.
}
\keyword{internal}
\examples{
.morie_gh_hellinger(p = 0.5, q = 0.5, grid = c(1, 2, 3, 4, 5, 6, 7, 8))
}
