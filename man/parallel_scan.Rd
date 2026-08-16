% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{parallel_scan}
\alias{parallel_scan}
\title{Blelloch scan over the affine composition}
\usage{
parallel_scan(pairs, x0 = 0)
}
\arguments{
\item{pairs}{See Usage.}
\item{x0}{Defaults to \code{0}.}
}
\description{
Same states as sequential_scan, O(log L) depth.
}
\value{
A list with \code{states}, \code{prefix}, \code{depth}, \code{work}, \code{note}.
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
