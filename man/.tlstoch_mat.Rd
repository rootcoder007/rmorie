% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.tlstoch_mat}
\alias{.tlstoch_mat}
\title{.tlstoch_mat}
\usage{
.tlstoch_mat(x)
}
\arguments{
\item{x}{Optional; may be \code{NULL}. A vector; its length is taken and its elements indexed.}
}
\description{
A step of the tlstoch_native implementation. Called by \code{.tlstoch_density_ratio}, \code{.tlstoch_shift_tmle}, \code{.tlstoch_stochastic_estimand}. See the file header for the source the module follows.
}
\value{
A matrix, from \code{matrix}.
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
