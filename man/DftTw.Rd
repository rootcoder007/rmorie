% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{DftTw}
\alias{DftTw}
\title{Eq (3.83): the same transform written with twiddle factors, which is}
\usage{
DftTw(x)
}
\arguments{
\item{x}{See Usage.}
}
\description{
the structure the FFT exploits via eqs (3.88)-(3.89).  Checked against Dft() rather than assumed equal.
}
\value{
A list with \code{X}, \code{W}, \code{n}, \code{max_difference}, \code{agrees_with_definition}, \code{method}.
}
\keyword{internal}
