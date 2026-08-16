% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_check_numvec}
\alias{.morie_check_numvec}
\title{Internal helper: Morie Check Numvec}
\usage{
.morie_check_numvec(x, arg = "x", finite = TRUE, min_len = 1L)
}
\description{
Internal helper: Morie Check Numvec
}
\value{
The value of \code{x}, as built in the body.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.morie_check_numvec(V)
}
