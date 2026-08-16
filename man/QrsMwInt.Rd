% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{QrsMwInt}
\alias{QrsMwInt}
\title{Eq (4.15).  N matters: too wide merges QRS and T, too narrow leaves}
\usage{
QrsMwInt(x, nwin = 30, fs = NULL)
}
\arguments{
\item{x}{See Usage.}
\item{nwin}{Defaults to \code{30}.}
\item{fs}{Defaults to \code{NULL}.}
}
\description{
multiple peaks.  The book found N = 30 at fs = 200 Hz, i.e. 150 ms, and supplying fs preserves the 150 ms window at any rate.
}
\value{
A list with \code{y}, \code{nwin}, \code{widthsec}, \code{n}, \code{method}.
}
\keyword{internal}
