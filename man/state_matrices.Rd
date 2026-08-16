% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{state_matrices}
\alias{state_matrices}
\title{state_matrices}
\usage{
state_matrices(spec, theta)
}
\arguments{
\item{spec}{Passed to \code{.flatten_carry}.}
\item{theta}{See Usage.}
}
\description{
A step of the bats_native implementation. Called by \code{all_eigenvalues}, \code{spectral_radius}. See the file header for the source the module follows.
}
\value{
A list with \code{w}, \code{fmat}, \code{g}.
}
\keyword{internal}
