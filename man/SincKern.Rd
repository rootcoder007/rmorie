% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{SincKern}
\alias{SincKern}
\title{The inverse transform of a rectangular passband, truncated to M+1}
\usage{
SincKern(fc, fs = 1000, M = 64, window = NULL) 
}
\arguments{
\item{fc}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{fs}{Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{1000}.}
\item{M}{Coerced to integer by the body, with \code{as.integer}. Defaults to \code{64}.}
\item{window}{Optional; may be \code{NULL}. Carried through into a list the body builds.}
}
\description{
taps and delayed by M/2 to make it causal.  Truncation is multiplying by a rectangle, whose sidelobes decay slowly, so the realized stopband ripples: Gibbs\' phenomenon, and it does not improve with M -- only the ripples narrow, they do not shrink.  A window fixes it.
}
\value{
A list with \code{h}, \code{n_taps}, \code{fc}, \code{fs}, \code{M}, \code{window}, \code{window_values}, \code{delay_samples}, \code{dc_gain}, \code{truncation_causes_gibbs_ripple}, \code{ripple_height_does_not_shrink_with_M}, \code{method}.
}
\keyword{internal}
\examples{
SincKern(fc = 5L)
}
