% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{Edfstep}
\alias{Edfstep}
\title{Empirical distribution function -- eq. (2.3.1), p. 32}
\usage{
Edfstep(x, t) 
}
\description{
Empirical distribution function -- eq. (2.3.1), p. 32
}
\arguments{
\item{x}{Argument \code{x}; see Usage.}
\item{t}{Argument \code{t}; see Usage.}
}
\value{
A list with \code{edf}, \code{count}, \code{n}, \code{sorted}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
Edfstep(V, V)
}
