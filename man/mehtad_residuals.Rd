% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{mehtad_residuals}
\alias{mehtad_residuals}
\title{mehtad_residuals}
\usage{
mehtad_residuals(A, b, c, x, y, s)
}
\arguments{
\item{A}{See Usage.}
\item{b}{See Usage.}
\item{c}{See Usage.}
\item{x}{See Usage.}
\item{y}{See Usage.}
\item{s}{See Usage.}
}
\description{
Part of the mehtad_native implementation; see the file header for the source it follows.
}
\value{
A list with \code{primal}, \code{dual}, \code{mu}, \code{primal_norm}, \code{dual_norm}, \code{note}.
}
\keyword{internal}
\examples{
mehtad_residuals(A = c(1, 2, 3, 4, 5, 6, 7, 8), b = 5L, c = c(1, 2, 3, 4, 5, 6, 7, 8), x = c(1, 2, 3, 4, 5, 6, 7, 8), y = c(1, 2, 3, 4, 5, 6, 7, 8), s = c(1, 2, 3, 4, 5, 6, 7, 8))
}
