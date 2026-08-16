% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.robpca_directions}
\alias{.robpca_directions}
\title{.robpca_directions}
\usage{
.robpca_directions(rows, n_dirs, seed)
}
\arguments{
\item{rows}{A matrix; indexed by row and column.}
\item{n_dirs}{Numeric; combined arithmetically in the body.}
\item{seed}{Passed to \code{.ghc_rng}.}
}
\description{
A step of the robpca_native implementation. Called by \code{outlyingness}. See the file header for the source the module follows.
}
\value{
The value of \code{do.call}.
}
\keyword{internal}
\examples{
.robpca_directions(rows = matrix(c(1, 2, 3, 4, 5, 6), nrow = 2), n_dirs = 5L, seed = 5L)
}
