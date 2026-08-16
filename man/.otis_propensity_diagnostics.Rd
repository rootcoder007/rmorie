% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.otis_propensity_diagnostics}
\alias{.otis_propensity_diagnostics}
\title{Internal helper: Otis Propensity Diagnostics}
\usage{
.otis_propensity_diagnostics(p, d)
}
\description{
Internal helper: Otis Propensity Diagnostics
}
\value{
A list with \code{brier}, \code{obs_prevalence}, \code{predicted_prevalence}, \code{log_loss}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.otis_propensity_diagnostics(V, V)
}
