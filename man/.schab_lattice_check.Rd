% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.schab_lattice_check}
\alias{.schab_lattice_check}
\title{.schab_lattice_check}
\usage{
.schab_lattice_check(z)
}
\arguments{
\item{z}{A matrix; passed to \code{nrow}.}
}
\description{
A step of the schab_rest_shared implementation. Called by \code{.schab_periodogram}, \code{.schab_periodogram_from_cov}, \code{.schab_sample_cov2d}. See the file header for the source the module follows.
}
\value{
The value of \code{z}, as built in the body.
}
\keyword{internal}
\examples{
M <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 2)
.schab_lattice_check(M)
}
