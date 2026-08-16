% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.funmix_cholsolve}
\alias{.funmix_cholsolve}
\title{.funmix_cholsolve}
\usage{
.funmix_cholsolve(A, b)
}
\arguments{
\item{A}{A matrix; passed to \code{chol}.}
\item{b}{See Usage.}
}
\description{
A step of the funmix_native implementation. Called by \code{morie_funmix_functional_mixture}. See the file header for the source the module follows.
}
\value{
A vector, from \code{as.numeric}.
}
\keyword{internal}
