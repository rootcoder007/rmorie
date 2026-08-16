% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{smcsam}
\alias{smcsam}
\title{smcsam}
\usage{
smcsam(log_gamma, initial, n_particles = 500L, ladder = NULL, n_steps = 20L, kernel = NULL, ess_threshold = 0.5, scheme = "systematic", seed = 0L, weight_rule = "mcmc", log_forward = NULL, log_backward = NULL)
}
\arguments{
\item{log_gamma}{See Usage.}
\item{initial}{See Usage.}
\item{n_particles}{Defaults to \code{500L}.}
\item{ladder}{Defaults to \code{NULL}.}
\item{n_steps}{Defaults to \code{20L}.}
\item{kernel}{Defaults to \code{NULL}.}
\item{ess_threshold}{Defaults to \code{0.5}.}
\item{scheme}{Defaults to \code{"systematic"}.}
\item{seed}{Defaults to \code{0L}.}
\item{weight_rule}{One of \code{"general"}, \code{"mcmc"}. Defaults to \code{"mcmc"}.}
\item{log_forward}{Defaults to \code{NULL}.}
\item{log_backward}{Defaults to \code{NULL}.}
}
\description{
A step of the smcsam_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{estimate}, \code{mean}, \code{variance}, \code{particles}, \code{weights}, \code{log_norm_const}, \code{ess}, \code{ess_trace}, \code{resampled}, \code{accept_trace}, \code{ladder}, \code{n_particles}, \code{weight_rule}, \code{method}.
}
\keyword{internal}
