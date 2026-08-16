% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.schab_st_is_valid_covariance}
\alias{.schab_st_is_valid_covariance}
\title{Eq (9.5) is positive semi-definiteness. Checked by}
\usage{
.schab_st_is_valid_covariance(coords, times, cov_fn, tol = NULL)
}
\arguments{
\item{coords}{Passed to \code{.schab_st_covariance_matrix}.}
\item{times}{Passed to \code{.schab_st_covariance_matrix}.}
\item{cov_fn}{Passed to \code{.schab_st_covariance_matrix}.}
\item{tol}{Optional; may be \code{NULL}. Carried through into a list the body builds.}
}
\description{
eigendecomposition, not by sampling random coefficient vectors: the minimum eigenvalue IS the minimum of the quadratic form, so one eigensolve settles what no finite number of random draws can. Sec. 9.3 records that Gneiting (2002) found published covariance functions in Cressie and Huang (1999) to be invalid, so construction alone is not proof.
}
\value{
A list with \code{valid}, \code{min_eigenvalue}, \code{max_eigenvalue}, \code{tolerance}, \code{reason}.
}
\keyword{internal}
