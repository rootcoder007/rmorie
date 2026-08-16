% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{Sift}
\alias{Sift}
\title{Empirical mode decomposition by sifting, Sec 9.4 steps 1-6}
\usage{
Sift(x, max_imfs = 10, tol = 0.05)
}
\arguments{
\item{x}{See Usage.}
\item{max_imfs}{Defaults to \code{10}.}
\item{tol}{Defaults to \code{0.05}.}
}
\description{
Part of the rangayyan_tf implementation; see the file header for the source it follows.
}
\value{
A list with \code{imfs}, \code{residual}, \code{n_imfs}, \code{reconstruction_error}, \code{energy_per_imf}, \code{tol}, \code{method}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
Sift(V)
}
