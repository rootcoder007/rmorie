% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_scan_interval}
\alias{morie_scan_interval}
\alias{morie_interval_mapping}
\title{LOD scan along an interval}
\usage{
morie_scan_interval(y, left, right, length, step = 0.01, ...) 

morie_interval_mapping(y, left, right, length, step = 0.01, ...) 
}
\arguments{
\item{y}{Numeric phenotype vector.}
\item{left}{Numeric vector of left-marker genotypes.}
\item{right}{Numeric vector of right-marker genotypes.}
\item{length}{Length of the interval in Morgans, positive.}
\item{step}{Step size in Morgans (default 0.01).}
\item{...}{Forwarded to \code{morie_interval_map}   (\code{max_iter}, \code{tol}).}
}
\description{
Walks a putative QTL along the interval between the two flanking markers and reports the LOD at every position, the position with the maximum LOD, and the fit at that position.
}
\value{
A named list with keys \code{estimate}, \code{peak_lod},   \code{peak_position}, \code{position}, \code{lod}, \code{fit},   \code{method}.
}
\keyword{internal}
\examples{
set.seed(1)
n <- 50
g1 <- rbinom(n, 1, 0.5)
g2 <- ifelse(runif(n) < 0.85, g1, 1 - g1)
y <- 1 + 0.9 * g1 + rnorm(n, 0, 0.5)
r <- morie_scan_interval(y, g1, g2, length = 0.15, step = 0.05)
str(r, max.level = 1)
}
