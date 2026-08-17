% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_bayoptr}
\alias{morie_bayoptr}
\title{Bayesian optimisation, chosen by acquisition function}
\usage{
morie_bayoptr(f, bounds, acquisition = "ei", n_iter = 20L, n_init = 5L, kernel = "matern52", amplitude = 1, length_scale = 1, noise = 1e-08, kappa = 2, xi = 0, n_candidates = 200L, seed = 0L, X0 = NULL, y0 = NULL)
}
\arguments{
\item{f}{Objective \code{function(x)} to minimise; \code{x} is a   numeric vector of length \code{length(bounds)}.}
\item{bounds}{List of \code{c(low, high)} pairs, one per   parameter.}
\item{acquisition}{One of \code{"ei"}, \code{"expected_improvement"},   \code{"pi"}, \code{"probability_of_improvement"}, \code{"ucb"},   \code{"lcb"} or \code{"confidence_bound"}. \code{"ucb"} and   \code{"lcb"} name the same rule -- the paper writes the lower   bound because it minimises.}
\item{n_iter}{Number of optimisation iterations after the initial   design.}
\item{n_init}{Number of initial uniform random points.}
\item{kernel}{Kernel name; only \code{"matern52"} is implemented.}
\item{amplitude,length_scale,noise}{GP hyperparameters.}
\item{kappa}{Exploration weight for the LCB rule.}
\item{xi}{Exploration slack for the EI / PI rules.}
\item{n_candidates}{Number of L-BFGS-B starts per iteration.}
\item{seed}{Seed for the shared generator.}
\item{X0,y0}{Optional initial design and values; if given,   \code{n_init} is ignored.}
}
\description{
Mirrors Python \code{morie.fn.bayoptr.bayoptr}: it resolves an acquisition spelling, calls the underlying optimiser, and appends the "ucb"/"lcb" note to the result. As \code{n_iter} grows the posterior on \code{f} sharpens around the observed points and the acquisition selects the next point; the trade-off between exploration and exploitation is governed by the acquisition choice and its \code{kappa} / \code{xi}.
}
\value{
Named list whose element names match the Python payload   keys, including \code{best_x}, \code{best_y}, \code{X}, \code{y},   \code{x_trace}, \code{y_trace}, the hyperparameters, \code{acq},   \code{acquisition} (the spelling the caller used), and   \code{note} (the underlying optimiser\'s note with the   "ucb"/"lcb" suffix appended).
}
\keyword{internal}
\examples{
f <- function(x) -sum((x - c(0.3, 0.7))^2)
morie_bayoptr(f, bounds = list(c(0, 1), c(0, 1)), n_iter = 8L, n_init = 5L,
              seed = 1L)
}
