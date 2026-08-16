% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.bnppct_wq}
\alias{.bnppct_wq}
\title{Weighted empirical quantile: the smallest point whose cumulative}
\usage{
.bnppct_wq(ys_sorted, weights, q)
}
\arguments{
\item{ys_sorted}{See Usage.}
\item{weights}{See Usage.}
\item{q}{See Usage.}
}
\description{
weight reaches q. The inverse of the weighted ECDF, which is a step function, so nothing is interpolated between the steps.
}
\value{
The value of \code{[}.
}
\references{
  Kottas, A. and Krnjajic, M. (2009) "Bayesian semiparametric
    modelling in quantile regression." Scandinavian Journal of
    Statistics 36(2), 297-319.
  Rubin, D.B. (1981) "The Bayesian bootstrap." Annals of Statistics
    9(1), 130-134.
  Walker (2007); Kalli, Griffin and Walker (2011) -- the sampler,
    through morie_slbpdg.
}
\keyword{internal}
