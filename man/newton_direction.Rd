% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{newton_direction}
\alias{newton_direction}
\title{newton_direction}
\usage{
newton_direction(A, x, s, rp, rd, rc)
}
\arguments{
\item{A}{See Usage.}
\item{x}{See Usage.}
\item{s}{See Usage.}
\item{rp}{See Usage.}
\item{rd}{See Usage.}
\item{rc}{See Usage.}
}
\description{
Part of the mehtad_native implementation; see the file header for the source it follows.
}
\value{
A list with \code{dx}, \code{dy}, \code{ds}.
}
\keyword{internal}
\examples{
newton_direction(A = c(1, 2, 3, 4, 5, 6, 7, 8), x = 5L, s = 5L, rp = c(1, 2, 3, 4, 5, 6, 7, 8), rd = c(1, 2, 3, 4, 5, 6, 7, 8), rc = c(1, 2, 3, 4, 5, 6, 7, 8))
}
