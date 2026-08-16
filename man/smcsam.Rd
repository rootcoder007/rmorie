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
\item{log_gamma}{Accepted by the signature and not used anywhere in the body.}
\item{initial}{Accepted by the signature and not used anywhere in the body.}
\item{n_particles}{Coerced to integer by the body, with \code{as.integer}. Defaults to \code{500L}.}
\item{ladder}{Optional; may be \code{NULL}. Coerced to numeric by the body, with \code{as.numeric}.}
\item{n_steps}{Passed to \code{temperature_ladder}. Defaults to \code{20L}.}
\item{kernel}{Optional; may be \code{NULL}. Passed to \code{is.null}.}
\item{ess_threshold}{Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{0.5}.}
\item{scheme}{Passed to \code{resample}. Defaults to \code{"systematic"}.}
\item{seed}{Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{0L}.}
\item{weight_rule}{One of \code{"general"}, \code{"mcmc"}. Defaults to \code{"mcmc"}.}
\item{log_forward}{Optional; may be \code{NULL}. Passed to \code{is.null}.}
\item{log_backward}{The body requires: log_backward densities (equation 12).}
}
\description{
A step of the smcsam_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{estimate}, \code{mean}, \code{variance}, \code{particles}, \code{weights}, \code{log_norm_const}, \code{ess}, \code{ess_trace}, \code{resampled}, \code{accept_trace}, \code{ladder}, \code{n_particles}, \code{weight_rule}, \code{method}.
}
\keyword{internal}
