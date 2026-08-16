% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.clusmd_rich}
\alias{.clusmd_rich}
\title{.clusmd_rich}
\usage{
.clusmd_rich(title, summary_lines = list(), tables = list(), interpretation = "", warnings = character(), payload = list())
}
\arguments{
\item{title}{See Usage.}
\item{summary_lines}{Defaults to \code{list()}.}
\item{tables}{Defaults to \code{list()}.}
\item{interpretation}{Defaults to \code{""}.}
\item{warnings}{Coerced to character by the body, with \code{as.character}. Defaults to \code{character()}.}
\item{payload}{A vector; its length is taken and its elements indexed. Defaults to \code{list()}.}
}
\description{
Public entry point: cluster fingerprints by exclusion sphere. --------------------------------------------------------------------------- Returns a rich-result-shaped named list whose `$payload` carries the fields the Python arm puts in its RichResult payload, mirroring the key set exactly. The list itself (and `$payload`) is also a plain named list with the same names, so callers can use `result$size` or `result$payload$size` interchangeably, just like the Python arm.
}
\value{
The value of \code{out}, as built in the body.
}
\references{
----------
Butina, D. (1999) "Unsupervised data base clustering based on
Daylight\'s fingerprint and Tanimoto similarity: a fast and automated
way to cluster small and large data sets", *Journal of Chemical
Information and Computer Sciences* 39(4), 747-750,
doi:10.1021/ci9803381. The neighbour-count ordering, the exclusion
sphere around each selected centroid, the singleton handling, and the
0.8 Tanimoto working threshold reproduced as the default.

Willett, P., Barnard, J. M. & Downs, G. M. (1998) "Chemical
similarity searching", *Journal of Chemical Information and Computer
Sciences* 38(6), 983-996, doi:10.1021/ci9800211, for the Tanimoto
coefficient itself.

Native R port of morie.fn.clusmd. No external packages; the Tanimoto
coefficient on binary fingerprints is computed inline (c / (a + b - c))
so this arm is independent of sasimi_native.R.
}
\keyword{internal}
