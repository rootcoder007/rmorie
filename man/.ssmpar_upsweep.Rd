% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.ssmpar_upsweep}
\alias{.ssmpar_upsweep}
\title{Build the tree of partial compositions (private helper)}
\usage{
.ssmpar_upsweep(P)
}
\arguments{
\item{P}{See Usage.}
}
\description{
A step of the ssmpar_native implementation. Called by \code{parallel_scan}. See the file header for the source the module follows.
}
\value{
The value of \code{tree}, as built in the body.
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
.ssmpar_upsweep(V)
}
