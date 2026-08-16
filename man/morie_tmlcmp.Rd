% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_tmlcmp}
\alias{morie_tmlcmp}
\title{morie_tmlcmp}
\usage{
morie_tmlcmp(time, event_type, D, X, times = NULL, cause = 1, horizon = NULL, g = NULL, iters = 50)
}
\arguments{
\item{time}{Passed to \code{.tmlcmp_vec}.}
\item{event_type}{Passed to \code{.tmlcmp_vec}.}
\item{D}{Passed to \code{.tmlcmp_vec}.}
\item{X}{Passed to \code{.tmlcmp_mat}.}
\item{times}{Defaults to \code{NULL}.}
\item{cause}{Defaults to \code{1}.}
\item{horizon}{Defaults to \code{NULL}.}
\item{g}{Defaults to \code{NULL}.}
\item{iters}{Defaults to \code{50}.}
}
\description{
A step of the tmlcmp_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{estimate}, \code{psi}, \code{F_treated}, \code{F_control}, \code{curve_treated}, \code{curve_control}, \code{se}, \code{ci}, \code{horizon}, \code{cause}, \code{times}, \code{closure_treated}, \code{method}, \code{note}.
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
