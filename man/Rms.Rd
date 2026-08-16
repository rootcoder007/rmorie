% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{Rms}
\alias{Rms}
\title{Eq (3.9): RMS = sqrt((1/N) sum x^2), divisor N.  With a window, the}
\usage{
Rms(x, window = NULL)
}
\arguments{
\item{x}{See Usage.}
\item{window}{Defaults to \code{NULL}.}
}
\description{
short-time RMS the book uses for EMG activity (Section 5.6).
}
\value{
The value of \code{out}, as built in the body.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
Rms(V)
}
