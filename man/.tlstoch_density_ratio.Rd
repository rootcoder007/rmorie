% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.tlstoch_density_ratio}
\alias{.tlstoch_density_ratio}
\title{.tlstoch_density_ratio}
\usage{
.tlstoch_density_ratio(A, W, delta, g_fn, lower = NULL, upper = NULL)
}
\arguments{
\item{A}{Passed to \code{.tlstoch_vec}.}
\item{W}{Passed to \code{.tlstoch_mat}.}
\item{delta}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{g_fn}{See Usage.}
\item{lower}{Defaults to \code{NULL}.}
\item{upper}{Defaults to \code{NULL}.}
}
\description{
A step of the tlstoch_native implementation. Called by \code{.tlstoch_shift_tmle}. See the file header for the source the module follows.
}
\value{
A list with \code{H}, \code{max}, \code{mean}.
}
\references{
----------
van der Laan, M. J. & Rose, S. (2018) Targeted Learning in Data
Science, Springer, doi:10.1007/978-3-319-65304-4. Chap. 14 (Diaz &
van der Laan): static and dynamic regimes are both deterministic
because they are completely determined by pre-treatment variables;
deterministic regimes are the wrong framework for phenomena not
subject to direct intervention -- the exercise-regime and mass-media
examples, where an intervention is deterministic at the community
level and stochastic at the individual level; causal effects for
deterministic regimes may be unidentifiable because the regime is not
supported in the observed data; the data, notation and parameter of
interest with its identification and positivity assumption; the
optimality theory for stochastic regimes; the TMLE and its asymptotic
distribution; and super learning for a conditional density as the
initial estimator.

Diaz, I. & van der Laan, M. J. (2012) "Population Intervention Causal
Effects Based on Stochastic Interventions", Biometrics 68(2), 541-549,
doi:10.1111/j.1541-0420.2011.01685.x.

Haneuse, S. & Rotnitzky, A. (2013) "Estimation of the effect of
interventions that modify the received treatment", Statistics in
Medicine 32(30), 5260-5277, doi:10.1002/sim.5907. Modified-treatment
policies.
}
\keyword{internal}
