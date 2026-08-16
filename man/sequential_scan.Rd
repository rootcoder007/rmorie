% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{sequential_scan}
\alias{sequential_scan}
\title{The recurrence as written: L steps, no parallelism}
\usage{
sequential_scan(pairs, x0 = 0)
}
\arguments{
\item{pairs}{A vector; its length is taken.}
\item{x0}{Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{0}.}
}
\description{
A step of the ssmpar_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{states}, \code{steps}, \code{depth}, \code{note}.
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
sequential_scan(V)
}
