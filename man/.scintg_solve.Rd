% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.scintg_solve}
\alias{.scintg_solve}
\title{.scintg_solve}
\usage{
.scintg_solve(A, B)
}
\arguments{
\item{A}{A matrix; passed to \code{nrow}.}
\item{B}{A matrix; passed to \code{ncol}.}
}
\description{
A step of the scintg_native implementation. Called by \code{.scintg_correct_batch}. See the file header for the source the module follows.
}
\value{
The value of \code{X}, as built in the body.
}
\keyword{internal}
