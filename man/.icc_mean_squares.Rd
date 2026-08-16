% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.icc_mean_squares}
\alias{.icc_mean_squares}
\title{The Shrout-Fleiss two-way ANOVA of Hedderich et al., pp. 427-428}
\usage{
.icc_mean_squares(rows, n, k)
}
\arguments{
\item{rows}{A matrix; indexed by row and column.}
\item{n}{A count; the body uses it as \code{seq_len(...)}.}
\item{k}{A count; the body uses it as \code{seq_len(...)}.}
}
\description{
A step of the icc2 implementation. Called by \code{Icc2}, \code{Icc3}. See the file header for the source the module follows.
}
\value{
A list with \code{bms}, \code{wms}, \code{jms}, \code{ems}.
}
\keyword{internal}
\examples{
.icc_mean_squares(rows = matrix(c(1, 2, 3, 4, 5, 6), nrow = 2), n = c(1, 2, 3, 4, 5, 6, 7, 8), k = 3L)
}
