% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_rapaf_partial_ar}
\alias{morie_rapaf_partial_ar}
\title{Partial attributable risk}
\usage{
morie_rapaf_partial_ar(case_counts, rate_ratios, baseline_map) 
}
\arguments{
\item{case_counts}{Non-negative case counts per stratum.}
\item{rate_ratios}{Positive rate ratios.}
\item{baseline_map}{Integer vector giving, for each stratum, the   index of the stratum it would become under the intervention.}
}
\description{
AR attributable to a subset of factors, with the other factors held at their stratum values; partial ARs are not additive.
}
\value{
A list with \code{estimate}, \code{ar}, \code{baseline_map},   \code{note}.
}
\keyword{internal}
