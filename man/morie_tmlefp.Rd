% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_tmlefp}
\alias{morie_tmlefp}
\title{Optimal-overlap estimands and their trimming rules}
\usage{
morie_tmlefp(y, treatment, pscore, sigma2_treated = NULL, sigma2_control = NULL, estimand = "ate")
}
\arguments{
\item{y}{Outcome vector.}
\item{treatment}{Treatment indicator.}
\item{pscore}{Propensity scores, strictly inside (0, 1).}
\item{sigma2_treated}{Optional conditional variance under treatment.}
\item{sigma2_control}{Optional conditional variance under control.}
\item{estimand}{One of \code{"ate"}, \code{"att"}.}
}
\description{
Optimal-overlap estimands and their trimming rules
}
\value{
A list with \code{estimate}, \code{osate}, \code{ate_full},   \code{owate}, \code{owate_weights}, \code{alpha}, \code{gamma},   \code{keep}, \code{n}, \code{n_kept}, \code{n_trimmed},   \code{no_trimming}, \code{variance_bound},   \code{variance_bound_full}, \code{estimand}, \code{note},   \code{method}.
}
\keyword{internal}
