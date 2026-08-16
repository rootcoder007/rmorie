% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{SmPgram}
\alias{SmPgram}
\title{Daniell (equal-weight) smoothing of the eq (4.57) periodogram.  NOT}
\usage{
SmPgram(y, span = 3L)
}
\arguments{
\item{y}{See Usage.}
\item{span}{Numeric; combined arithmetically in the body. Defaults to \code{3L}.}
}
\description{
in Schabenberger & Gotway: "Daniell" and "smoothed periodogram" are absent from the book, whose Sec. 4.7.2 fits a parametric spectral density instead.  See Bloomfield (2000), Fourier Analysis of Time Series, 2nd edn, Ch. 8.  The window is CIRCULAR; a truncating window would bias both ends.
}
\value{
A list with \code{omega}, \code{smoothed}, \code{raw}, \code{span}, \code{equivalent_df}, \code{circular_window}, \code{n}, \code{method}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
SmPgram(V)
}
