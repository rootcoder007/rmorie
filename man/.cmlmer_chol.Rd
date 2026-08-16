% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.cmlmer_chol}
\alias{.cmlmer_chol}
\title{.cmlmer_chol}
\usage{
.cmlmer_chol(A)
}
\arguments{
\item{A}{A matrix; indexed by row and column.}
}
\description{
A step of the cmlmer_native implementation. Called by \code{.cmlmer_reml_at}, \code{morie_cmlmer_compressed_lmm}. See the file header for the source the module follows.
}
\value{
The value of \code{L}, as built in the body.
}
\keyword{internal}
