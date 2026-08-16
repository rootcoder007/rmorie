% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.mehtad_cholsolve}
\alias{.mehtad_cholsolve}
\title{.mehtad_cholsolve}
\usage{
.mehtad_cholsolve(M, rhs)
}
\arguments{
\item{M}{A matrix; passed to \code{chol}.}
\item{rhs}{A matrix; passed to \code{solve}.}
}
\description{
A step of the mehtad_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A vector, from \code{as.numeric}.
}
\keyword{internal}
