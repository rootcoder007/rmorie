% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{Mwu}
\alias{Mwu}
\title{Mann-Whitney U -- Sec. 6.6, eqs. (6.6.1), (6.6.14), p. 260}
\usage{
Mwu(x, y) 
}
\description{
Mann-Whitney U -- Sec. 6.6, eqs. (6.6.1), (6.6.14), p. 260
}
\arguments{
\item{x}{Argument \code{x}; see Usage.}
\item{y}{Argument \code{y}; see Usage.}
}
\value{
A list with \code{statistic}, \code{p_value}, \code{z}, \code{p_normal}, \code{mean}, \code{var}, \code{m}, \code{n}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
Mwu(V, V)
}
