% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{Wsrsym}
\alias{Wsrsym}
\title{Signed-rank test of symmetry -- Sec. 5.7.7, p. 211}
\usage{
Wsrsym(x, centre = 0) 
}
\description{
Signed-rank test of symmetry -- Sec. 5.7.7, p. 211
}
\arguments{
\item{x}{Argument \code{x}; see Usage.}
\item{centre}{Argument \code{centre}; see Usage.}
}
\value{
A list with \code{statistic}, \code{z}, \code{p_value}, \code{mean}, \code{var}, \code{skewdir}, \code{n}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
rmorie:::Wsrsym(V)
}
