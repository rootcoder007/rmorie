% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_tps_kernel}
\alias{.morie_tps_kernel}
\title{.morie_tps_kernel}
\usage{
.morie_tps_kernel(A, B)
}
\arguments{
\item{A}{A matrix; passed to \code{tcrossprod}.}
\item{B}{A matrix; passed to \code{tcrossprod}.}
}
\description{
A step of the esl_native2 implementation. Called by \code{morie_esl_thin_plate_spline}. See the file header for the source the module follows.
}
\value{
The value of \code{out}, as built in the body.
}
\keyword{internal}
