% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_rdd_mccrary_native}
\alias{.morie_rdd_mccrary_native}
\title{Internal helper: McCrary (2008) density discontinuity test}
\usage{
.morie_rdd_mccrary_native(x, cutoff = 0, bin = NULL, bandwidth = NULL)
}
\description{
Histogram with the cutoff on a bin edge, then one-sided local linear fits to the normalized bin counts; the statistic is the log-difference of the density estimates at the cutoff with McCrary\'s asymptotic standard error.
}
\value{
A list with \code{statistic}, \code{theta}, \code{se}, \code{p_value}, \code{f_left}, \code{f_right}, \code{bandwidth}, \code{binsize}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.morie_rdd_mccrary_native(V)
}
