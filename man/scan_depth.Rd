% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{scan_depth}
\alias{scan_depth}
\title{Sequential against parallel depth}
\usage{
scan_depth(length)
}
\arguments{
\item{length}{See Usage.}
}
\description{
A step of the ssmpar_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{estimate}, \code{parallel_depth}, \code{sequential_depth}, \code{work}, \code{speedup}, \code{method}, \code{note}.
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
scan_depth(length = 5L)
}
