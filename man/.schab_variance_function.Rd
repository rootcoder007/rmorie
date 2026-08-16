% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.schab_variance_function}
\alias{.schab_variance_function}
\title{.schab_variance_function}
\usage{
.schab_variance_function(mu, family)
}
\arguments{
\item{mu}{A vector; its length is taken.}
\item{family}{The body requires: `family` must be 'poisson', 'binomial' or 'gaussian'.}
}
\description{
A step of the schab_glmm_shared implementation. Called by \code{.schab_conditional_variance}, \code{.schab_data_covariance}, \code{.schab_sigma_mu}. See the file header for the source the module follows.
}
\value{
Nothing; this branch always raises.
}
\keyword{internal}
