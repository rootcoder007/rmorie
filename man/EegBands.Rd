% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{EegBands}
\alias{EegBands}
\title{Band limits from Section 1.2.6, fractional power by eq (6.44) as used}
\usage{
EegBands(x, fs, bands = NULL)
}
\arguments{
\item{x}{See Usage.}
\item{fs}{See Usage.}
\item{bands}{Defaults to \code{NULL}.}
}
\description{
in Section 10.2.3.  The clinical question -- is there an alpha rhythm -- is answered by the fraction of power in that band, not by the power.
}
\value{
A list with \code{power}, \code{fraction}, \code{dominant}, \code{totalpower}, \code{frequencies}, \code{bands}, \code{method}.
}
\keyword{internal}
