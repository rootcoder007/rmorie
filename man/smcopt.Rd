% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{smcopt}
\alias{smcopt}
\title{smcopt}
\usage{
smcopt(objective, initial, n_particles = 200, n_steps = 30, phi_max = 50, phi_min = 0.1, kind = "geometric", kernel = NULL, ess_threshold = 0.5, scheme = "systematic", seed = 0, maximise = TRUE)
}
\arguments{
\item{objective}{See Usage.}
\item{initial}{See Usage.}
\item{n_particles}{Defaults to \code{200}.}
\item{n_steps}{Defaults to \code{30}.}
\item{phi_max}{Defaults to \code{50}.}
\item{phi_min}{Defaults to \code{0.1}.}
\item{kind}{Defaults to \code{"geometric"}.}
\item{kernel}{Defaults to \code{NULL}.}
\item{ess_threshold}{Defaults to \code{0.5}.}
\item{scheme}{Defaults to \code{"systematic"}.}
\item{seed}{Defaults to \code{0}.}
\item{maximise}{A flag; the body branches on it. Defaults to \code{TRUE}.}
}
\description{
A step of the smcopt_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{estimate}, \code{best_x}, \code{best_value}, \code{particles}, \code{weights}, \code{particle_mean}, \code{ladder}, \code{ess_trace}, \code{resampled}, \code{accept_trace}, \code{n_particles}, \code{maximise}, \code{note}, \code{method}.
}
\keyword{internal}
