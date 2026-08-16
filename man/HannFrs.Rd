% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{HannFrs}
\alias{HannFrs}
\title{Eq (3.105): a REAL nonnegative factor times a pure one-sample delay}
\usage{
HannFrs(omega)
}
\arguments{
\item{omega}{See Usage.}
}
\description{
That factorization is the point -- it proves the filter has exactly linear phase, so the waveform is shifted, not distorted.
}
\value{
A list with \code{H}, \code{omega}, \code{envelope}, \code{max_difference_from_eq_3_104}, \code{agrees_with_raw_form}, \code{real_factor_times_a_pure_delay}, \code{linear_phase}, \code{method}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
HannFrs(V)
}
