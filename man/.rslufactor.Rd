% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.rslufactor}
\alias{.rslufactor}
\title{LU with partial pivoting.  Returns list(M, piv, sign, singular).}
\usage{
.rslufactor(A)
}
\arguments{
\item{A}{A matrix; passed to \code{nrow}.}
}
\description{
Singularity is judged RELATIVE to the largest entry of A, not against exact zero.
}
\value{
A list with \code{M}, \code{piv}, \code{sign}, \code{singular}.
}
\keyword{internal}
