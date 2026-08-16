% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_fairness_favorable_rates}
\alias{.morie_fairness_favorable_rates}
\title{.morie_fairness_favorable_rates}
\usage{
.morie_fairness_favorable_rates(outcome, group, favorable)
}
\arguments{
\item{outcome}{A vector; indexed elementwise.}
\item{group}{Passed to \code{.morie_fairness_ordered_unique}.}
\item{favorable}{See Usage.}
}
\description{
A step of the frns_metrics implementation. Called by \code{morie_fairness_bias_amplification}, \code{morie_fairness_demographic_parity}, \code{morie_fairness_disparate_impact}. See the file header for the source the module follows.
}
\value{
The value of \code{rates}, as built in the body.
}
\keyword{internal}
