% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_didm_point}
\alias{.morie_didm_point}
\title{Internal helper: the DID-M point estimator on group-level panel data}
\usage{
.morie_didm_point(df, outcome, treatment, unit, time)
}
\description{
Instantaneous treatment effect for switchers: for every pair of consecutive periods, compares outcome changes of joiners (0 to 1) with stable-at-0 groups and leavers (1 to 0) with stable-at-1 groups, weighting by switcher counts.
}
\value{
A numeric value.
}
\keyword{internal}
