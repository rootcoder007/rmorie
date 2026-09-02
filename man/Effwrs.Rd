% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{Effwrs}
\alias{Effwrs}
\title{Mann-Whitney / rank-sum efficacy -- eq. (13.3.10), p. 494}
\usage{
Effwrs(m, n, integral) 
}
\description{
Mann-Whitney / rank-sum efficacy -- eq. (13.3.10), p. 494
}
\arguments{
\item{m}{Argument \code{m}; see Usage.}
\item{n}{Argument \code{n}; see Usage.}
\item{integral}{Argument \code{integral}; see Usage.}
}
\value{
A list with \code{efficacy}, \code{integral}, \code{m}, \code{n}.
}
\keyword{internal}
\examples{
Effwrs(m = 5L, n = 5L, integral = c(1, 2, 3, 4, 5, 6, 7, 8))
}
