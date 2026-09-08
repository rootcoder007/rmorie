% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{Wwties}
\alias{Wwties}
\title{Runs-test tie bounds -- Sec. 6.2.1, p. 233}
\usage{
Wwties(x, y) 
}
\description{
Runs-test tie bounds -- Sec. 6.2.1, p. 233
}
\arguments{
\item{x}{Argument \code{x}; see Usage.}
\item{y}{Argument \code{y}; see Usage.}
}
\value{
A list with \code{rmin}, \code{rmax}, \code{nties}, \code{ambiguous}, \code{m}, \code{n}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
rmorie:::Wwties(V, V)
}
