% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.rfgain}
\alias{.rfgain}
\title{The (15.6) objective, MAXIMISED -- see the erratum recorded in}
\usage{
.rfgain(Y, left, right, q)
}
\arguments{
\item{Y}{A matrix; indexed by row and column.}
\item{left}{A vector; its length is taken.}
\item{right}{A vector; its length is taken.}
\item{q}{A count; the body uses it as \code{seq_len(...)}.}
}
\description{
rfmlt.R Part of the mvsml_rf_shared implementation; see the file header for the source it follows.
}
\value{
The value of \code{g}, as built in the body.
}
\keyword{internal}
\examples{
.rfgain(Y = c(1, 2, 3, 4, 5, 6, 7, 8), left = c(1, 2, 3, 4, 5, 6, 7, 8), right = c(1, 2, 3, 4, 5, 6, 7, 8), q = 0.5)
}
