% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_bayhmc}
\alias{morie_bayhmc}
\title{No-U-Turn Sampler with dual-averaging step-size adaptation}
\usage{
morie_bayhmc(logp, theta0, n_iter = 1000L, warmup = NULL, grad = NULL, sampler = "nuts", delta = 0.65, eps = NULL, n_steps = 10L, max_depth = 10L, gamma = 0.05, t0 = 10, kappa = 0.75, seed = 0L)
}
\arguments{
\item{logp}{Function \code{(theta)} returning the log density   up to an additive constant.}
\item{theta0}{Numeric vector, the starting state.}
\item{n_iter}{Total number of iterations (warmup + sampling).}
\item{warmup}{Number of warmup iterations; defaults to   \code{n_iter \%/\% 2} when \code{NULL}.}
\item{grad}{Function \code{(theta)} returning the gradient of   \code{logp}; when \code{NULL} a central-difference numerical   gradient is used.}
\item{sampler}{One of \code{"nuts"} (the default) or   \code{"hmc"} (fixed-length Algorithm 1, needs   \code{n_steps}).}
\item{delta}{Target average Metropolis acceptance, in (0, 1).   Default 0.65, the paper\'s recommended value.}
\item{eps}{Optional step size; if \code{NULL} it is set by   Algorithm 4 (find-reasonable-epsilon) before the chain   starts.}
\item{n_steps}{Leapfrog steps per iteration for the HMC   baseline.}
\item{max_depth}{Maximum tree depth (doublings) for NUTS.}
\item{gamma,t0,kappa}{Dual-averaging hyperparameters; the   paper\'s defaults are 0.05, 10, 0.75.}
\item{seed}{Seed for the shared generator.}
}
\description{
Draws from an unnormalised target by Hamiltonian Monte Carlo, with the trajectory length set by the No-U-Turn criterion and the step size set by Nesterov dual averaging (Hoffman and Gelman 2014). \code{logp} is the log density up to an additive constant; supply \code{grad} or fall back to a central- difference numerical gradient. Set \code{sampler = "hmc"} for the fixed-length Algorithm 1 baseline.
}
\value{
A list with \code{estimate}, \code{samples},   \code{mean}, \code{variance}, \code{sd}, \code{acceptance},   \code{eps}, \code{eps_trace}, \code{depths}, \code{n_samples},   \code{warmup}, \code{sampler}, \code{delta}, \code{method},   \code{note}.
}
\keyword{internal}
\examples{
logp <- function(t) -0.5 * sum(t^2)
r <- morie_bayhmc(logp, c(1, -1), n_iter = 200L, seed = 1)
str(r, max.level = 1)
}
