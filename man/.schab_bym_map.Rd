% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.schab_bym_map}
\alias{.schab_bym_map}
\title{.schab_bym_map}
\usage{
.schab_bym_map(y, c_exp, adjacency, kappa, lam, max_iter = 200L, tol = 1e-11)
}
\arguments{
\item{y}{See Usage.}
\item{c_exp}{See Usage.}
\item{adjacency}{See Usage.}
\item{kappa}{See Usage.}
\item{lam}{See Usage.}
\item{max_iter}{Defaults to \code{200L}.}
\item{tol}{Defaults to \code{1e-11}.}
}
\description{
Part of the schab_glmm_shared implementation; see the file header for the source it follows.
}
\value{
A list with \code{u}, \code{v}, \code{x}, \code{relative_risk}, \code{fitted}, \code{n_iter}, \code{converged}, \code{sum_v}, \code{fitted_total}, \code{observed_total}, \code{log_posterior}.
}
\keyword{internal}
