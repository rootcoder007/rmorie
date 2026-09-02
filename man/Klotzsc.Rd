% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{Klotzsc}
\alias{Klotzsc}
\title{Klotz normal-scores scale test -- eq. (9.5.1), p. 322}
\usage{
Klotzsc(x, y) 
}
\description{
Klotz normal-scores scale test -- eq. (9.5.1), p. 322
}
\arguments{
\item{x}{Argument \code{x}; see Usage.}
\item{y}{Argument \code{y}; see Usage.}
}
\value{
A list with \code{statistic}, \code{mean}, \code{var}, \code{z}, \code{p_value}, \code{scores}, \code{m}, \code{n}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
rmorie:::Klotzsc(V, V)
}
