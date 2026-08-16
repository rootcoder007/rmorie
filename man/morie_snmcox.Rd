% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_snmcox}
\alias{morie_snmcox}
\title{G-estimate psi in the structural nested failure time model.}
\usage{
morie_snmcox(time, event, treatment_history, covariate_history = NULL, treat_times = NULL, censor_time = NULL, level = 0.95, psi_range = c(-3, 3), n_grid = 241L, tol = 1e-10, ridge = 1e-08)
}
\description{
G-estimate psi in the structural nested failure time model.
}
\value{
A list with \code{estimate}, \code{psi}, \code{time_ratio}, \code{lower}, \code{upper}, \code{score_at_estimate}, \code{z_at_estimate}, \code{n_used}, \code{artificial_censored}, \code{blipped}, \code{propensity}, \code{converged}, \code{grid_psi}, \code{grid_score}, \code{n}, \code{level}, \code{method}, \code{note}.
}
\references{
Robins, J. M. (1992) "Estimation of the time-dependent accelerated failure
time model in the presence of confounding factors", Biometrika 79(2),
321-334, doi:10.1093/biomet/79.2.321.
Robins, J. M., Blevins, D., Ritter, G. and Wulfsohn, M. (1992) "G-estimation
of the effect of prophylaxis therapy for Pneumocystis carinii pneumonia on
the survival of AIDS patients", Epidemiology 3(4), 319-336,
doi:10.1097/00001648-199207000-00007.
Hernan, M. A., Cole, S. R., Margolick, J., Cohen, M. and Robins, J. M. (2005)
"Structural accelerated failure time models for survival analysis in studies
with time-varying treatments", Pharmacoepidemiology and Drug Safety 14(7),
477-491, doi:10.1002/pds.1064.
}
\keyword{internal}
\examples{
morie_snmcox(time = c(1, 2, 3, 4, 5, 6, 7, 8), event = c(0, 1, 0, 1, 1, 0, 1, 0), treatment_history = c(1, 2, 3, 4, 5, 6, 7, 8))
}
