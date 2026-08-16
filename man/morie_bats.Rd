% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_bats}
\alias{morie_bats}
\title{BATS and TBATS exponential smoothing}
\usage{
morie_bats(y, seasonal_periods = numeric(0), harmonics = NULL, use_box_cox = NULL, use_trend = NULL, damped = NULL, p = 0L, q = 0L, long_run_b = 0, h = 0L, maxiter = 2000L)
}
\arguments{
\item{y}{Numeric series; strictly positive when \code{use_box_cox}   is TRUE.}
\item{seasonal_periods}{Numeric vector of seasonal periods, each   exceeding 1.}
\item{harmonics}{NULL for the BATS index seasonal of eq. 3e, or a   vector of harmonic counts for TBATS (eq. 4a-4c). A length-zero   vector fills in the index-equivalent defaults for each period;   NA entries fall back to the default for that period.}
\item{use_box_cox}{NULL tries both TRUE and FALSE and keeps the   lower AIC; TRUE or FALSE fixes the choice.}
\item{use_trend}{NULL tries both; TRUE or FALSE fixes the choice.}
\item{damped}{NULL tries both when \code{use_trend} is TRUE;   TRUE or FALSE fixes the choice.}
\item{p,q}{ARMA orders, non-negative integers.}
\item{long_run_b}{Long-run trend value; the short-run trend   converges to this rather than to zero.}
\item{h}{Forecast horizon.}
\item{maxiter}{Maximum Nelder-Mead iterations per starting point.}
}
\description{
Equations 3a-3f (BATS) or with the trigonometric seasonal of 4a-4c (TBATS) of De Livera, Hyndman & Snyder (2010). The seed state is concentrated out by least squares rather than searched, the concentrated log-likelihood carries the Box-Cox Jacobian, and the optimiser is constrained to the forecastability region of Hyndman et al. (2007).
}
\value{
A named list matching the Python payload keys:   \code{model, omega, phi, alpha, beta, gamma, ar, ma, seed_state,   fitted, fitted_transformed, residuals, loglik, aic, n_par,   sigma2, spectral_radius, forecastable, forecast,   forecast_transformed, candidates, spec, method, note}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
morie_bats(V)
}
