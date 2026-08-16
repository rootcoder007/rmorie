% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{Parseval}
\alias{Parseval}
\title{Eq (3.91), discrete form: sum |x(n)|^2 = (1/N) sum |X(k)|^2.  With}
\usage{
Parseval(x)
}
\arguments{
\item{x}{See Usage.}
}
\description{
the unnormalized forward transform of eq (3.80) the spectral sum is N times the time-domain energy, so omitting the 1/N inflates it by the record length.
}
\value{
A list with \code{energy_time}, \code{energy_freq}, \code{psd}, \code{max_difference}, \code{holds}, \code{n}, \code{method}.
}
\keyword{internal}
