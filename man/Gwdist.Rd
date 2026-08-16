% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{Gwdist}
\alias{Gwdist}
\title{Gromov-Wasserstein discrepancy (Memoli 2011)}
\usage{
Gwdist(Cx, Cy, a, b, n_iter = 50, epsilon = 0.05, n_sinkhorn = 50)
}
\description{
Gromov-Wasserstein discrepancy (Memoli 2011)
}
\value{
A list with \code{T}, \code{cost}, \code{cost_product}, \code{n_iter}, \code{n}, \code{m}.
}
\keyword{internal}
\examples{
set.seed(5)
Cx <- as.matrix(dist(matrix(rnorm(10), 5, 2)))
Cy <- as.matrix(dist(matrix(rnorm(10), 5, 2)))
Gwdist(Cx, Cy, a = rep(0.2, 5), b = rep(0.2, 5), n_iter = 10)
}
