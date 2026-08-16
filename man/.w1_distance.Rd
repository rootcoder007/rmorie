% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.w1_distance}
\alias{.w1_distance}
\title{Internal: 1-Wasserstein distance between two empirical measures on}
\usage{
.w1_distance(a, b)
}
\arguments{
\item{a}{Numeric; passed to \code{sort}.}
\item{b}{Numeric; passed to \code{sort}.}
}
\description{
the line. On R the p = 1 transport cost is integral |F - G|, so sorting solves the problem and no linear program is needed.
}
\value{
A numeric value.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.w1_distance(V, V)
}
