% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.lcwphr_cholsolve}
\alias{.lcwphr_cholsolve}
\title{.lcwphr_cholsolve}
\usage{
.lcwphr_cholsolve(A, b)
}
\arguments{
\item{A}{A matrix; passed to \code{chol}.}
\item{b}{Passed to \code{forwardsolve}.}
}
\description{
A step of the lcwphr_native implementation. Called by \code{.lcwphr_logit_irls}. See the file header for the source the module follows.
}
\value{
A vector, from \code{as.numeric}.
}
\keyword{internal}
