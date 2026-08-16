% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.rfbest}
\alias{.rfbest}
\title{.rfbest}
\usage{
.rfbest(X, Y, rows, cand, q)
}
\arguments{
\item{X}{A matrix; indexed by row and column.}
\item{Y}{Passed to \code{.rfgain}.}
\item{rows}{A vector; indexed elementwise.}
\item{cand}{See Usage.}
\item{q}{Passed to \code{.rfgain}.}
}
\description{
A step of the mvsml_rf_shared implementation. Called by \code{.rfgrow}. See the file header for the source the module follows.
}
\value{
The value of \code{best}, as built in the body.
}
\keyword{internal}
