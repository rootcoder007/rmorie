% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_compare_sets}
\alias{morie_compare_sets}
\title{morie_compare_sets}
\usage{
morie_compare_sets(phi_hat, half_width, se_phi, level = 0.95, conditional_prior = NULL, n_grid = 401L)
}
\arguments{
\item{phi_hat}{See Usage.}
\item{half_width}{See Usage.}
\item{se_phi}{See Usage.}
\item{level}{Defaults to \code{0.95}.}
\item{conditional_prior}{Defaults to \code{NULL}.}
\item{n_grid}{Defaults to \code{401L}.}
}
\description{
Part of the bndbye_native implementation; see the file header for the source it follows.
}
\value{
A list with \code{estimate}, \code{identified_set}, \code{credible_hpd}, \code{confidence_set}, \code{hpd_inside_identified_set}, \code{cs_contains_identified_set}, \code{width_ratio_hpd_over_cs}, \code{conditional_prior_reported}, \code{method}, \code{recommendation}.
}
\keyword{internal}
