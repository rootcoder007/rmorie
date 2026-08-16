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
\examples{
.morie_causal_forest_ate(nf = data.frame(x = c(1, 2, 3, 4), y = c(2, 4, 5, 9)), y = c(1, 2, 3, 4, 5, 6, 7, 8), w = c(1, 2, 3, 4, 5, 6, 7, 8))
}
