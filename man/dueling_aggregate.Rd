% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{dueling_aggregate}
\alias{dueling_aggregate}
\title{Duel aggregation: combine a value and an advantage vector into Q(s, .)}
\usage{
dueling_aggregate(value, advantage, mode = "mean")
}
\arguments{
\item{value}{Scalar value V(s).}
\item{advantage}{Numeric vector of advantages A(s, .).}
\item{mode}{One of \code{"mean"} (eq. 9, default), \code{"max"}   (eq. 8) or \code{"naive"}.}
}
\description{
Implements the recombination rules of Wang et al. (2016) for the dueling network architecture.  Eq. (9) (the default) subtracts the mean of the advantage vector; Eq. (8) subtracts the max; the naive form \code{Q = V + A} is unidentifiable and is exposed under \code{mode = "naive"} only so its failure can be demonstrated.
}
\value{
Numeric vector of Q(s, .).
}
\keyword{internal}
