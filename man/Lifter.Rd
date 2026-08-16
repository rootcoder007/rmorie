% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{Lifter}
\alias{Lifter}
\title{Section 4.7.3: the vocal tract lives at LOW quefrency, the glottal}
\usage{
Lifter(cepstrum_values, low = NULL, high = NULL, keep = "low")
}
\arguments{
\item{cepstrum_values}{See Usage.}
\item{low}{Defaults to \code{NULL}.}
\item{high}{Defaults to \code{NULL}.}
\item{keep}{Defaults to \code{"low"}.}
}
\description{
excitation at the pitch period and its multiples.  The window is applied SYMMETRICALLY about zero quefrency, because the cepstrum of a mixed-phase signal is two-sided (eq 4.72) and keeping only the causal half would discard the maximum-phase component.
}
\value{
A list with \code{liftered}, \code{n}, \code{low}, \code{high}, \code{keep}, \code{symmetric}, \code{n_kept}, \code{energy_kept}, \code{method}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
Lifter(V)
}
