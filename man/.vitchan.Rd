% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.vitchan}
\alias{.vitchan}
\title{Read an image as a list of C matrices, each H-by-W.  A plain H-by-W}
\usage{
.vitchan(image)
}
\arguments{
\item{image}{Passed to \code{.s03mat}.}
}
\description{
matrix is the single-channel case, C = 1.
}
\value{
The value of \code{out}, as built in the body.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.vitchan(V)
}
