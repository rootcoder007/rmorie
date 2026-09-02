% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{Fftperiod}
\alias{Fftperiod}
\title{Dominant periods from the amplitude spectrum (Wu et al. 2023, TimesNet)}
\usage{
Fftperiod(x, k = 1) 
}
\description{
Dominant periods from the amplitude spectrum (Wu et al. 2023, TimesNet)
}
\arguments{
\item{x}{Argument \code{x}; see Usage.}
\item{k}{Argument \code{k}; see Usage.}
}
\value{
A list with \code{frequency}, \code{period}, \code{amplitude}, \code{spectrum}, \code{n}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
Fftperiod(V)
}
