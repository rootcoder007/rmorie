% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_w4d_lloyd}
\alias{.morie_w4d_lloyd}
\title{.morie_w4d_lloyd}
\usage{
.morie_w4d_lloyd(Z, k, seed = 0, iters = 100)
}
\arguments{
\item{Z}{A matrix; indexed by row and column.}
\item{k}{Numeric; combined arithmetically in the body.}
\item{seed}{Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{0}.}
\item{iters}{A count; the body uses it as \code{seq_len(...)}. Defaults to \code{100}.}
}
\description{
A step of the geron_w4d_native implementation. Called by \code{morie_geron_semisupervised_cluster}, \code{morie_geron_spectral_clustering}. See the file header for the source the module follows.
}
\value{
A list with \code{labels}, \code{centers}.
}
\keyword{internal}
\examples{
.morie_w4d_lloyd(Z = matrix(c(1, 2, 3, 4, 5, 6), nrow = 2), k = 5L)
}
