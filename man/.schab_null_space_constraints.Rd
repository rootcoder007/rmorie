% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.schab_null_space_constraints}
\alias{.schab_null_space_constraints}
\title{.schab_null_space_constraints}
\usage{
.schab_null_space_constraints(R_delta, tol = NULL)
}
\arguments{
\item{R_delta}{A matrix; passed to \code{as.matrix}.}
\item{tol}{Optional; may be \code{NULL}. Passed to \code{is.null}.}
}
\description{
A step of the schab_glmm_shared implementation. Called by \code{spbayr}. See the file header for the source the module follows.
}
\value{
A list with \code{A}, \code{e}, \code{n_constraints}, \code{rank_deficiency}.
}
\keyword{internal}
\examples{
.schab_null_space_constraints(R_delta = 5L)
}
