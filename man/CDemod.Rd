% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{CDemod}
\alias{CDemod}
\title{Complex demodulation, Sec 5.5.1 eqs (5.16)-(5.19)}
\usage{
CDemod(x, fs = 1, f0 = NULL, bandwidth = NULL)
}
\arguments{
\item{x}{See Usage.}
\item{fs}{Defaults to \code{1}.}
\item{f0}{Defaults to \code{NULL}.}
\item{bandwidth}{Defaults to \code{NULL}.}
}
\description{
Part of the rangayyan_tf implementation; see the file header for the source it follows.
}
\value{
A list with \code{amplitude}, \code{phase}, \code{demodulated}, \code{f0}, \code{bandwidth}, \code{mean_amplitude}, \code{method}.
}
\keyword{internal}
