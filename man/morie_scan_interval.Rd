% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_scan_interval}
\alias{morie_scan_interval}
\title{LOD scan along an interval}
\usage{
morie_scan_interval(y, left, right, length, step = 0.01, ...)
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
