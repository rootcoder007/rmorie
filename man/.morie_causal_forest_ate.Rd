% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_causal_forest_ate}
\alias{.morie_causal_forest_ate}
\title{Internal helper: AIPW ATE over a target sample from forest nuisances}
\usage{
.morie_causal_forest_ate(nf, y, w, target_sample = "all")
}
\description{
Internal helper: AIPW ATE over a target sample from forest nuisances
}
\value{
A list with \code{ate}, \code{se}.
}
\keyword{internal}
