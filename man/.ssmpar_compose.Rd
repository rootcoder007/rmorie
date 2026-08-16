% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.ssmpar_compose}
\alias{.ssmpar_compose}
\title{(A2, b2) o (A1, b1) = (A2*A1, A2*b1 + b2)}
\usage{
.ssmpar_compose(left, right)
}
\arguments{
\item{left}{A vector; indexed elementwise.}
\item{right}{A vector; indexed elementwise.}
}
\description{
A step of the ssmpar_native implementation. Called by \code{.ssmpar_upsweep}, \code{check_associativity}, \code{parallel_scan}. See the file header for the source the module follows.
}
\value{
A vector, from \code{c}.
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
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.ssmpar_compose(V, V)
}
