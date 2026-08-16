% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{check_associativity}
\alias{check_associativity}
\title{Test (a o b) o c == a o (b o c) directly}
\usage{
check_associativity(a, b, c, tol = 1e-12)
}
\arguments{
\item{a}{See Usage.}
\item{b}{See Usage.}
\item{c}{See Usage.}
\item{tol}{Defaults to \code{1e-12}.}
}
\description{
Part of the ssmpar_native implementation; see the file header for the source it follows.
}
\value{
A list with \code{left}, \code{right}, \code{deviation}, \code{associative}, \code{note}.
}
\references{
----------
Smith, J. T. H., Warrington, A. & Linderman, S. W. (2023)
"Simplified State Space Layers for Sequence Modeling",
International Conference on Learning Representations (ICLR 2023),
arXiv:2208.04933.
Gu, A. & Dao, T. (2024) "Mamba: Linear-Time Sequence Modeling with
Selective State Spaces", Conference on Language Modeling (COLM 2024),
arXiv:2312.00752.
Blelloch, G. E. (1990) "Prefix Sums and Their Applications",
Technical Report CMU-CS-90-190, School of Computer Science, Carnegie
Mellon University.
}
\keyword{internal}
\examples{
check_associativity(a = c(1, 2, 3, 4, 5, 6, 7, 8), b = 5L, c = c(1, 2, 3, 4, 5, 6, 7, 8))
}
