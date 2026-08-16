% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.schab_bym_icar_log_prior}
\alias{.schab_bym_icar_log_prior}
\title{.schab_bym_icar_log_prior}
\usage{
.schab_bym_icar_log_prior(u, adjacency, kappa)
}
\arguments{
\item{u}{A matrix; passed to \code{t}.}
\item{adjacency}{Passed to \code{.schab_neighbour_structure}.}
\item{kappa}{Numeric; passed to \code{log}.}
}
\description{
A step of the schab_glmm_shared implementation. Called by \code{.schab_bym_log_posterior}. See the file header for the source the module follows.
}
\value{
A numeric value.
}
\keyword{internal}
