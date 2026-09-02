% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{SchwarzC}
\alias{SchwarzC}
\title{Eq (4.42), with equality exactly when A = K B* -- which, applied with}
\usage{
SchwarzC(A, B, grid) 
}
\arguments{
\item{A}{Coerced to complex by the body, with \code{as.complex}.}
\item{B}{Coerced to complex by the body, with \code{as.complex}.}
\item{grid}{Coerced to numeric by the body, with \code{as.numeric}.}
}
\description{
A = H and B = X exp(+j2 pi f t0), IS the matched-filter derivation.
}
\value{
A list with \code{lhs}, \code{rhs}, \code{holds}, \code{ratio}, \code{equality}, \code{k}, \code{collinear}, \code{method}.
}
\keyword{internal}
\examples{
SchwarzC(A = c(1, 2, 3, 4, 5, 6, 7, 8), B = c(1, 2, 3, 4, 5, 6, 7, 8), grid = c(1, 2, 3, 4, 5, 6, 7, 8))
}
