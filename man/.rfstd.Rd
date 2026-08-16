% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.rfstd}
\alias{.rfstd}
\title{The p. 656 requirement: responses standardized before splitting}
\usage{
.rfstd(Y, n, q)
}
\arguments{
\item{Y}{A matrix; indexed by row and column.}
\item{n}{A count; the body uses it as \code{matrix(...)}.}
\item{q}{A count; the body uses it as \code{seq_len(...)}.}
}
\description{
A step of the mvsml_rf_shared implementation. Called by \code{Rfmdi}, \code{Rfmlt}, \code{Rfpmi}. See the file header for the source the module follows.
}
\value{
The value of \code{out}, as built in the body.
}
\keyword{internal}
\examples{
.rfstd(Y = c(1, 2, 3, 4, 5, 6, 7, 8), n = 5L, q = 0.5)
}
