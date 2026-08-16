% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_gr_lstsq}
\alias{.morie_gr_lstsq}
\title{Minimum-norm least squares, matching numpy.linalg.lstsq (SVD based),}
\usage{
.morie_gr_lstsq(A, b)
}
\arguments{
\item{A}{A matrix; passed to \code{ncol}.}
\item{b}{Coerced to numeric by the body, with \code{as.numeric}.}
}
\description{
which differs from qr.solve on rank-deficient designs.
}
\value{
A vector, from \code{as.numeric}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.morie_gr_lstsq(V, V)
}
