% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_fairness_resolve_privileged}
\alias{.morie_fairness_resolve_privileged}
\title{.morie_fairness_resolve_privileged}
\usage{
.morie_fairness_resolve_privileged(privileged, rates, warnings_env)
}
\arguments{
\item{privileged}{Optional; may be \code{NULL}. Coerced to character by the body, with \code{as.character}.}
\item{rates}{Iterated over elementwise, with \code{vapply}.}
\item{warnings_env}{A list; the body reads \code{$w} from it.}
}
\description{
A step of the frns_metrics implementation. Called by \code{morie_fairness_average_odds_difference}, \code{morie_fairness_bias_amplification}, \code{morie_fairness_demographic_parity} and 2 others in the module. See the file header for the source the module follows.
}
\value{
The value of \code{inferred}, as built in the body.
}
\keyword{internal}
