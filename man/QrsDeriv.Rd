% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{QrsDeriv}
\alias{QrsDeriv}
\title{Balda et al., Section 4.3.1: y0 by eq (4.1), y1 by eq (4.2), combined}
\usage{
QrsDeriv(x, fs, thresh = 1)
}
\arguments{
\item{x}{See Usage.}
\item{fs}{See Usage.}
\item{thresh}{Defaults to \code{1}.}
}
\description{
as y2 = 1.3 y0 + 1.1 y1 (eq 4.3), scanned with a threshold of 1.0 on a maximum-normalised ECG.  Six of eight successive samples must pass, which is what makes the decision robust to single-sample derivative spikes.
}
\value{
A list with \code{qrs}, \code{y0}, \code{y1}, \code{y2}, \code{y3}, \code{mask}, \code{thresh}, \code{fs}, \code{hr}, \code{method}.
}
\keyword{internal}
