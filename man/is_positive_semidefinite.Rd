% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{is_positive_semidefinite}
\alias{is_positive_semidefinite}
\title{Test whether a matrix is positive semi-definite}
\usage{
is_positive_semidefinite(S, tol = -1e-09) 
}
\arguments{
\item{S}{Square symmetric matrix.}
\item{tol}{Numerical tolerance.}
}
\description{
Test whether a matrix is positive semi-definite
}
\value{
List with eigenvalues, min_eigenvalue, psd.
}
\keyword{internal}
\examples{
is_positive_semidefinite(S = 5L)
}
