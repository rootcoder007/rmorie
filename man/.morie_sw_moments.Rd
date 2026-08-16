% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_sw_moments}
\alias{.morie_sw_moments}
\title{Strasser-Weber conditional moments of the linear statistic}
\usage{
.morie_sw_moments(g, h)
}
\arguments{
\item{g}{See Usage.}
\item{h}{See Usage.}
}
\description{
g : n x p transformation of the covariate (e.g. group indicators) h : n x q influence (transformation) of the response Returns the vec\\\'d statistic T (pq), its conditional mean mu and covariance Sigma, matching coin\\\'s `expectation()` / `covariance()`.
}
\value{
A list with \code{T}, \code{mu}, \code{Sigma}, \code{n}.
}
\keyword{internal}
