% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.smatch_cholsolve}
\alias{.smatch_cholsolve}
\title{Symmetric positive-definite solve via base R\\'s chol}
\usage{
.smatch_cholsolve(M, b)
}
\arguments{
\item{M}{A matrix; passed to \code{chol}.}
\item{b}{See Usage.}
}
\description{
A step of the smatch_native implementation. Called by \code{morie_smatch_sccs_poisson_fit}. See the file header for the source the module follows.
}
\value{
A vector, from \code{as.numeric}.
}
\keyword{internal}
\examples{
.smatch_cholsolve(M = 5L, b = 5L)
}
