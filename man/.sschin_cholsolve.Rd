% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.sschin_cholsolve}
\alias{.sschin_cholsolve}
\title{.sschin_cholsolve}
\usage{
.sschin_cholsolve(A, b)
}
\arguments{
\item{A}{A matrix; passed to \code{chol}.}
\item{b}{Passed to \code{forwardsolve}.}
}
\description{
A step of the sschin_native implementation. Called by \code{.sschin_cox_breslow}, \code{.sschin_ols}. See the file header for the source the module follows.
}
\value{
A vector, from \code{as.numeric}.
}
\keyword{internal}
