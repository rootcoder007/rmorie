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
\item{y}{A vector; its length is taken.}
\item{c_exp}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{adjacency}{Passed to \code{.schab_neighbour_structure}.}
\item{kappa}{Numeric; combined arithmetically in the body.}
\item{lam}{Numeric; combined arithmetically in the body.}
\item{max_iter}{Coerced to integer by the body, with \code{as.integer}. Defaults to \code{200L}.}
\item{tol}{Passed to \code{<}. Defaults to \code{1e-11}.}
}
\description{
A step of the schab_glmm_shared implementation. Called by \code{spbym}. See the file header for the source the module follows.
}
\value{
A list with \code{u}, \code{v}, \code{x}, \code{relative_risk}, \code{fitted}, \code{n_iter}, \code{converged}, \code{sum_v}, \code{fitted_total}, \code{observed_total}, \code{log_posterior}.
}
\keyword{internal}
