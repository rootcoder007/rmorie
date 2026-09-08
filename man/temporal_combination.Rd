% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{temporal_combination}
\alias{temporal_combination}
\title{Temporal combination across aggregation levels}
\usage{
temporal_combination(y, levels, horizon = 1L, method = "tsb", alpha = 0.1, beta = 0.05, 
    weights = NULL) 
}
\arguments{
\item{y}{Numeric demand series.}
\item{levels}{Integer vector of bucket sizes (at least two).}
\item{horizon}{Forecast horizon in periods.}
\item{method}{Base forecaster method name.}
\item{alpha}{Size smoothing.}
\item{beta}{Probability smoothing.}
\item{weights}{Optional combination weights; default uniform.}
}
\description{
Petropoulos & Kourentzes: rather than choosing one aggregation level, forecast at several and combine, which removes the level-selection decision and is more robust than any single level chosen in advance.
}
\value{
Named list: \code{estimate}, \code{forecast}, \code{levels},   \code{per_level}, \code{weights}, \code{spread}, \code{method}.
}
\keyword{internal}
\examples{
temporal_combination(y = c(1, 2, 3, 4, 5, 6, 7, 8), levels = factor(c("lo", "hi", "lo", "hi")))
}
