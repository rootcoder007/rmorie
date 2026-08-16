% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.w3_nelder_mead}
\alias{.w3_nelder_mead}
\title{Nelder-Mead simplex minimisation. Derivative-free and deterministic}
\usage{
.w3_nelder_mead(f, x0, step = 0.1, iters = 400L, alpha = 1, gamma = 2, rho = 0.5, sigma = 0.5)
}
\arguments{
\item{f}{See Usage.}
\item{x0}{See Usage.}
\item{step}{Defaults to \code{0.1}.}
\item{iters}{Defaults to \code{400L}.}
\item{alpha}{Defaults to \code{1}.}
\item{gamma}{Defaults to \code{2}.}
\item{rho}{Defaults to \code{0.5}.}
\item{sigma}{Defaults to \code{0.5}.}
}
\description{
given the starting point, which is what a likelihood with a step-function selection term needs -- the objective is not differentiable at the cutoffs, so a gradient method would be answering a question the model does not pose. The initial simplex is the standard one: x0 plus a step along each coordinate, scaled by the coordinate when it is non-zero so the simplex is not tiny in a large parameter and huge in a small one. A fixed iteration count keeps the arms in step.
}
\value{
A list with \code{x}, \code{value}.
}
\keyword{internal}
