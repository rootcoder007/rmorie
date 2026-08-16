% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.scintg_matrix}
\alias{.scintg_matrix}
\title{.scintg_matrix}
\usage{
.scintg_matrix(Z)
}
\arguments{
\item{Z}{A matrix; passed to \code{nrow}.}
}
\description{
A step of the scintg_native implementation. Called by \code{.scintg_correct_batch}, \code{.scintg_maximum_diversity_clustering}, \code{morie_scintg}. See the file header for the source the module follows.
}
\value{
The value of \code{Z}, as built in the body.
}
\keyword{internal}
