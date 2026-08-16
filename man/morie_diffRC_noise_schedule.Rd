% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_diffRC_noise_schedule}
\alias{morie_diffRC_noise_schedule}
\title{A linear noise schedule, scaled DOWN}
\usage{
morie_diffRC_noise_schedule(T, scale = 0.001, beta_min = 1e-04, beta_max = 0.02)
}
\arguments{
\item{T}{Integer number of diffusion steps.}
\item{scale}{Numeric scale factor on the betas.}
\item{beta_min}{Numeric lower bound on beta.}
\item{beta_max}{Numeric upper bound on beta.}
}
\description{
A linear \code{beta} schedule as in Ho et al. (2020), scaled by \code{scale}. At image-diffusion scales the personal history is destroyed, and with it the thing being predicted, so the scale is what the module is about.
}
\value{
A list with \code{beta}, \code{alpha_bar}, \code{T},   \code{scale}, \code{signal_retained} and a note.
}
\keyword{internal}
