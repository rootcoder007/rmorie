% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_diffRC_importance_weights}
\alias{morie_diffRC_importance_weights}
\title{Importance-sample the timesteps that actually carry loss}
\usage{
morie_diffRC_importance_weights(step_losses, uniform = FALSE, smoothing = 0.1)
}
\arguments{
\item{step_losses}{Numeric vector of per-step losses.}
\item{uniform}{Logical, return uniform weights.}
\item{smoothing}{Numeric additive smoothing.}
}
\description{
Uniform sampling spends most of its budget where the model is already right.
}
\value{
A list with \code{weights} and \code{effective_steps}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
morie_diffRC_importance_weights(V)
}
