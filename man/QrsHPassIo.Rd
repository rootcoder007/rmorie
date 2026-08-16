% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{QrsHPassIo}
\alias{QrsHPassIo}
\title{Eq (4.13): p(n) = p(n-1) - (1/32) x(n) + x(n-16) - x(n-17) +}
\usage{
QrsHPassIo(x)
}
\arguments{
\item{x}{See Usage.}
}
\description{
(1/32) x(n-32).  Eqs (4.9)-(4.12) folded into one recursion: four adds per sample and a single state variable.
}
\value{
A list with \code{p}, \code{n}, \code{delayms}, \code{fsnote}, \code{method}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
QrsHPassIo(V)
}
