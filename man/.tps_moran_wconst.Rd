% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.tps_moran_wconst}
\alias{.tps_moran_wconst}
\title{Internal helper: Tps Moran Weight Constants}
\usage{
.tps_moran_wconst(W)
}
\description{
The three weight sums every Moran/Geary moment is built from (Cliff & Ord 1981): \deqn{S_0 = \sum_i \sum_j w_{ij}, \quad       S_1 = \tfrac{1}{2}\sum_i \sum_j (w_{ij} + w_{ji})^2, \quad       S_2 = \sum_i \left(\sum_j w_{ij} + \sum_j w_{ji}\right)^2.} \eqn{S_1} is computed from the symmetric part because \eqn{\tfrac{1}{2}\sum(w_{ij}+w_{ji})^2 = 2\sum \bar{w}_{ij}^2} with \eqn{\bar{W} = (W + W\')/2} -- row-standardised weights are not symmetric, so using \eqn{W} directly would be wrong.
}
\value{
A list with \code{S0}, \code{S1}, \code{S2}.
}
\keyword{internal}
\examples{
D <- data.frame(x = c(1, 2, 3, 4), y = c(2, 4, 5, 9))
.tps_moran_wconst(D)
}
