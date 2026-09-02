% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{Placement}
\alias{Placement}
\title{Placements of Y among the X order statistics -- Sec. 2.11, p. 65}
\usage{
Placement(x, y) 
}
\description{
Placements of Y among the X order statistics -- Sec. 2.11, p. 65
}
\arguments{
\item{x}{Argument \code{x}; see Usage.}
\item{y}{Argument \code{y}; see Usage.}
}
\value{
A list with \code{placements}, \code{ranks}, \code{blocks}, \code{total}, \code{m}, \code{n}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
Placement(V, V)
}
