% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.schab_interaction_structure}
\alias{.schab_interaction_structure}
\title{.schab_interaction_structure}
\usage{
.schab_interaction_structure(R_space, R_time, kind)
}
\arguments{
\item{R_space}{A matrix; passed to \code{as.matrix}.}
\item{R_time}{A matrix; passed to \code{as.matrix}.}
\item{kind}{The body requires: `kind` must be one of I, II, III, IV.}
}
\description{
A step of the schab_glmm_shared implementation. Called by \code{spbayr}. See the file header for the source the module follows.
}
\value{
A list with \code{structure}, \code{kind}, \code{rank}, \code{rank_deficiency}, \code{n_constraints_required}.
}
\keyword{internal}
