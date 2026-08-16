% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_sccsno_fit}
\alias{morie_sccsno_fit}
\title{morie_sccsno_fit}
\usage{
morie_sccsno_fit(cases, risk_periods, age_breaks = c(), iters = 100, tol = 1e-10, ridge = 1e-10)
}
\arguments{
\item{cases}{See Usage.}
\item{risk_periods}{Passed to \code{.sccsno_rp}.}
\item{age_breaks}{Defaults to \code{c()}.}
\item{iters}{Defaults to \code{100}.}
\item{tol}{Defaults to \code{1e-10}.}
\item{ridge}{A matrix; passed to \code{diag}. Defaults to \code{1e-10}.}
}
\description{
A step of the sccsno_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{estimate}, \code{relative_incidence}, \code{log_ri}, \code{se_log_ri}, \code{age_effects}, \code{se_age}, \code{coef}, \code{se}, \code{loglik}, \code{n_cases}, \code{converged}, \code{iterations}, \code{n_risk_periods}, \code{n_age_bands}, \code{method}, \code{conditions_out}.
}
\references{
----------
Farrington, C. P. (1995) "Relative Incidence Estimation from Case
Series for Vaccine Safety Evaluation", Biometrics 51(1), 228-235.
JSTOR stable URL https://www.jstor.org/stable/2533328. Secs. 2-3.

Whitaker, H. J., Farrington, C. P., Spiessens, B. & Musonda, P.
(2006) "Tutorial in biostatistics: The self-controlled case series
method", Statistics in Medicine 25, 1768-1797, doi:10.1002/sim.2302.
}
\keyword{internal}
