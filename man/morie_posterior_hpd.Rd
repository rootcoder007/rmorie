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
\item{theta_set}{See Usage.}
\item{level}{Defaults to \code{0.95}.}
\item{conditional_prior}{Defaults to \code{NULL}.}
\item{n_grid}{Defaults to \code{401L}.}
}
\description{
Part of the bndbye_native implementation; see the file header for the source it follows.
}
\value{
A list with \code{lower}, \code{upper}, \code{width}, \code{level}, \code{covered}, \code{n_grid_points}, \code{method}.
}
\keyword{internal}
