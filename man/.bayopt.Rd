% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.bayopt}
\alias{.bayopt}
\title{Underlying optimiser (the Python arm\'s _bayopt)}
\usage{
.bayopt(f, bounds, n_iter = 20L, n_init = 5L, acq = "ei", kernel = "matern52", amplitude = 1, length_scale = 1, noise = 1e-08, kappa = 2, xi = 0, n_candidates = 200L, seed = 0L, X0 = NULL, y0 = NULL)
}
\description{
Mirrors what the Python module\'s _bayopt does: an initial design of \code{n_init} uniform points (or the user-supplied \code{X0} / \code{y0}), then \code{n_iter} rounds of drawing \code{n_candidates} uniform candidates, polishing each by L-BFGS-B on the chosen acquisition, picking the winner, and evaluating \code{f} there. The best observation so far is the incumbent that the next acquisition is computed against.
}
\value{
A list with \code{best_x}, \code{best_y}, \code{X}, \code{y}, \code{x_trace}, \code{y_trace}, \code{n_init}, \code{n_iter}, \code{acq}, \code{kernel}, \code{amplitude}, \code{length_scale}, \code{noise}, \code{kappa}, \code{xi}, \code{n_candidates}, \code{seed}, \code{note}.
}
\keyword{internal}
