% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{AbcdLemma}
\alias{AbcdLemma}
\title{Eq. (3.213), the matrix inversion lemma:}
\usage{
AbcdLemma(A, B, C, D)
}
\arguments{
\item{A}{A matrix; passed to \code{as.matrix}.}
\item{B}{A matrix; passed to \code{as.matrix}.}
\item{C}{A matrix; passed to \code{as.matrix}.}
\item{D}{A matrix; passed to \code{as.matrix}.}
}
\description{
(A + B C D)^-1 = A^-1 - A^-1 B (D A^-1 B + C^-1)^-1 D A^-1. Both sides are formed and compared.  Its value in RLS is that with k = 1 the only inverse left is of a SCALAR, which turns an O(M^3) inversion per sample into O(M^2).
}
\value{
A list with \code{direct}, \code{lemma}, \code{max_difference}, \code{holds}, \code{n}, \code{k}, \code{scalar_when_k_is_one}, \code{method}.
}
\keyword{internal}
\examples{
AbcdLemma(matrix(c(4, 1, 1, 3), 2, 2), matrix(c(1, 2), 2, 1), 
    matrix(1, 1, 1), matrix(c(1, 2), 1, 2))
}
