% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{cause_specific_hazards}
\alias{cause_specific_hazards}
\title{cause_specific_hazards}
\usage{
cause_specific_hazards(time, event_type, times, A = NULL, arm = NULL, weights = NULL)
}
\arguments{
\item{time}{See Usage.}
\item{event_type}{See Usage.}
\item{times}{See Usage.}
\item{A}{Defaults to \code{NULL}.}
\item{arm}{Defaults to \code{NULL}.}
\item{weights}{Defaults to \code{NULL}.}
}
\description{
Part of the tmlcmp_native implementation; see the file header for the source it follows.
}
\value{
A list with \code{hazards}, \code{types}, \code{times}, \code{n}.
}
\references{
----------
Rytgaard, H. C., Gerds, T. A. & van der Laan, M. J. (2022)
"Continuous-time targeted minimum loss-based estimation of
intervention-specific mean outcomes", The Annals of Statistics
50(5), 2469-2491, doi:10.1214/21-AOS2114, arXiv:2105.02088. The
generalisation of TMLE to time-varying interventions where
interventions, covariates and outcome occur at subject-specific
time-points on an arbitrarily fine time-scale. (The ledger previously
dated this 2023; the Annals publication is 2022.)

van der Laan, M. J. & Rose, S. (2018) Targeted Learning in Data
Science, Springer, doi:10.1007/978-3-319-65304-4. Chap. 11
(Benkeser, Carone & Gilbert): the competing risks framework with
each endpoint type a separate risk; cumulative incidence as the
cumulative parameter; the Aalen-Johansen estimator\'s consistency
under uninformative censoring and its efficiency absent covariates;
and the drawback that semiparametric hazard-based alternatives
require a correctly specified finite-dimensional regression model.

Aalen, O. O. & Johansen, S. (1978) "An Empirical Transition Matrix
for Non-Homogeneous Markov Chains Based on Censored Observations",
Scandinavian Journal of Statistics 5(3), 141-150.
}
\keyword{internal}
\examples{
cause_specific_hazards(time = c(1, 2, 3, 4, 5, 6, 7, 8), event_type = c(1, 2, 3, 4, 5, 6, 7, 8), times = c(1, 2, 3, 4, 5, 6, 7, 8))
}
