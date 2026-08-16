% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.scintg_harmony_objective}
\alias{.scintg_harmony_objective}
\title{.scintg_harmony_objective}
\usage{
.scintg_harmony_objective(Z, R, Y, batches, sigma = 0.1, theta = 2)
}
\arguments{
\item{Z}{A matrix; passed to \code{t}.}
\item{R}{A matrix; passed to \code{nrow}.}
\item{Y}{A matrix; passed to \code{\%*\%}.}
\item{batches}{Passed to \code{.scintg_cluster_batch_counts}.}
\item{sigma}{Numeric; combined arithmetically in the body. Defaults to \code{0.1}.}
\item{theta}{Numeric; combined arithmetically in the body. Defaults to \code{2}.}
}
\description{
A step of the scintg_native implementation. Called by \code{.scintg_maximum_diversity_clustering}. See the file header for the source the module follows.
}
\value{
A list with \code{total}, \code{fit}, \code{entropy}, \code{kl}.
}
\keyword{internal}
