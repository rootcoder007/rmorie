% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{SpecEntropy}
\alias{SpecEntropy}
\title{Eq (3.11) applied to the PSD normalized to unit mass.  Rangayyan}
\usage{
SpecEntropy(psd, freqs = NULL, fmin = NULL, fmax = NULL)
}
\arguments{
\item{psd}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{freqs}{Optional; may be \code{NULL}. Coerced to numeric by the body, with \code{as.numeric}.}
\item{fmin}{Optional; may be \code{NULL}. Passed to \code{is.null}.}
\item{fmax}{Optional; may be \code{NULL}. Passed to \code{is.null}.}
}
\description{
defines the spectral MOMENTS of Section 6.4.4 but prints no spectral-entropy equation, so this is said to be eq (3.11) applied to the PSD rather than quoted from the book.
}
\value{
A list with \code{entropy}, \code{units}, \code{max_entropy}, \code{normalized}, \code{n_bins}, \code{probabilities}, \code{method}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
SpecEntropy(V)
}
