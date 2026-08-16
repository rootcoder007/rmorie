% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{QrsDrvSmth}
\alias{QrsDrvSmth}
\title{Eq (4.5): the M-point moving average that collapses the several}
\usage{
QrsDrvSmth(g1, mwin = 8)
}
\arguments{
\item{g1}{See Usage.}
\item{mwin}{Defaults to \code{8}.}
}
\description{
derivative peaks across the Q-R-S swings into one pulse per beat.
}
\value{
A list with \code{g}, \code{mwin}, \code{n}, \code{method}.
}
\keyword{internal}
