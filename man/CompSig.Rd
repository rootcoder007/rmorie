% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{CompSig}
\alias{CompSig}
\title{A composite of shifted, scaled copies of one pattern.  Copies closer}
\usage{
CompSig(g, shifts, scales = NULL, n = NULL) 
}
\arguments{
\item{g}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{shifts}{Coerced to integer by the body, with \code{as.integer}.}
\item{scales}{Optional; may be \code{NULL}. Coerced to numeric by the body, with \code{as.numeric}.}
\item{n}{Optional; may be \code{NULL}. Coerced to integer by the body, with \code{as.integer}.}
}
\description{
together than the pattern is long OVERLAP, and the count is reported because an overlap is what breaks a peak-picking detector.
}
\value{
A list with \code{x}, \code{n}, \code{shifts}, \code{scales}, \code{peaks_expected_at}, \code{overlapping_pairs}, \code{pattern}, \code{pattern_length}, \code{n_copies}, \code{copies_add}, \code{overlap_breaks_peak_picking}, \code{method}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
CompSig(V, V)
}
