% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{FDiffFr}
\alias{FDiffFr}
\title{Eq (3.125).  The second form separates a half-sample delay from a}
\usage{
FDiffFr(omega, T = 1)
}
\arguments{
\item{omega}{See Usage.}
\item{T}{Defaults to \code{1}.}
}
\description{
real gain; the factor of j is what puts the phase a quarter turn ahead, the +pi/2 of eq (3.127).
}
\value{
A list with \code{H}, \code{omega}, \code{T}, \code{split_form}, \code{max_difference}, \code{forms_agree}, \code{half_sample_delay}, \code{method}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
FDiffFr(V)
}
