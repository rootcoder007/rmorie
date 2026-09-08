% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{IdftRI}
\alias{IdftRI}
\title{Eq (3.86): synthesis as a weighted sum of sinusoids.  The imaginary}
\usage{
IdftRI(X) 
}
\arguments{
\item{X}{Coerced to complex by the body, with \code{as.complex}.}
}
\description{
residue is reported, not discarded -- a large one means the spectrum was not conjugate-symmetric and the "real signal" reading is wrong.
}
\value{
A list with \code{x}, \code{complex}, \code{n}, \code{max_imaginary}, \code{method}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
IdftRI(V)
}
