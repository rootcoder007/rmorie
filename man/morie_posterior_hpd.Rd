% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_posterior_hpd}
\alias{morie_posterior_hpd}
\title{morie_posterior_hpd}
\usage{
morie_posterior_hpd(theta_set, level = 0.95, conditional_prior = NULL, n_grid = 401L)
}
\arguments{
\item{theta_set}{Passed to \code{morie_conditional_prior_uniform}.}
\item{level}{Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{0.95}.}
\item{conditional_prior}{Optional; may be \code{NULL}. Passed to \code{is.null}.}
\item{n_grid}{Passed to \code{morie_conditional_prior_uniform}. Defaults to \code{401L}.}
}
\description{
A step of the bndbye_native implementation. Called by \code{morie_compare_sets}. See the file header for the source the module follows.
}
\value{
A list with \code{lower}, \code{upper}, \code{width}, \code{level}, \code{covered}, \code{n_grid_points}, \code{method}.
}
\keyword{internal}
