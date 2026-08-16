% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.schab_bym_log_posterior}
\alias{.schab_bym_log_posterior}
\title{.schab_bym_log_posterior}
\usage{
.schab_bym_log_posterior(y, c_exp, u, v, kappa, lam, adjacency, epsilon = 0.01)
}
\arguments{
\item{y}{A vector; its length is taken.}
\item{c_exp}{See Usage.}
\item{u}{A vector; its length is taken.}
\item{v}{A vector; its length is taken.}
\item{kappa}{Numeric; combined arithmetically in the body.}
\item{lam}{Numeric; passed to \code{log}.}
\item{adjacency}{Passed to \code{.schab_bym_icar_log_prior}.}
\item{epsilon}{Numeric; combined arithmetically in the body. Defaults to \code{0.01}.}
}
\description{
A step of the schab_glmm_shared implementation. Called by \code{.schab_bym_map}. See the file header for the source the module follows.
}
\value{
A numeric value.
}
\keyword{internal}
