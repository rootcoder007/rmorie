% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{Rosenbm}
\alias{Rosenbm}
\title{Rosenbaum outside-extremes scale test -- eq. (9.9.2), p. 329}
\usage{
Rosenbm(x, y) 
}
\description{
Rosenbaum outside-extremes scale test -- eq. (9.9.2), p. 329
}
\arguments{
\item{x}{Argument \code{x}; see Usage.}
\item{y}{Argument \code{y}; see Usage.}
}
\value{
A list with \code{statistic}, \code{p_value}, \code{pmf}, \code{ymin}, \code{ymax}, \code{mean}, \code{m}, \code{n}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
rmorie:::Rosenbm(V, V)
}
