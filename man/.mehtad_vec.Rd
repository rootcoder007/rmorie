% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.mehtad_vec}
\alias{.mehtad_vec}
\title{.mehtad_vec}
\usage{
.mehtad_vec(v)
}
\arguments{
\item{v}{Passed to \code{unlist}.}
}
\description{
A step of the mehtad_native implementation. Called by \code{mehtad_residuals}. See the file header for the source the module follows.
}
\value{
A vector, from \code{as.numeric}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.mehtad_vec(V)
}
