% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.scintg_correct_batch}
\alias{.scintg_correct_batch}
\title{.scintg_correct_batch}
\usage{
.scintg_correct_batch(Z, R, batches, lam = 1, reference = NULL)
}
\arguments{
\item{Z}{Passed to \code{.scintg_matrix}.}
\item{R}{A matrix; indexed by row and column.}
\item{batches}{A vector; its length is taken.}
\item{lam}{Numeric; combined arithmetically in the body. Defaults to \code{1}.}
\item{reference}{Optional; may be \code{NULL}. A vector; its length is taken and its elements indexed.}
}
\description{
A step of the scintg_native implementation. Called by \code{morie_scintg}. See the file header for the source the module follows.
}
\value{
A list with \code{Z}, \code{W}, \code{batches}.
}
\keyword{internal}
