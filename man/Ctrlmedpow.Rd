% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{Ctrlmedpow}
\alias{Ctrlmedpow}
\title{Control median test power -- Sec. 6.5.2, p. 258}
\usage{
Ctrlmedpow(m, n, d, h, nodes = 2001) 
}
\description{
Control median test power -- Sec. 6.5.2, p. 258
}
\arguments{
\item{m}{Argument \code{m}; see Usage.}
\item{n}{Argument \code{n}; see Usage.}
\item{d}{Argument \code{d}; see Usage.}
\item{h}{Argument \code{h}; see Usage.}
\item{nodes}{Argument \code{nodes}; see Usage.}
}
\value{
A list with \code{power}, \code{pmf}, \code{q}, \code{r}, \code{m}, \code{n}, \code{d}.
}
\keyword{internal}
\examples{
rmorie:::Ctrlmedpow(m = 8, n = 9, d = 2, h = function(v) v^2)
}
