% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.vec}
\alias{.vec}
\title{.vec}
\usage{
.vec(x)
}
\arguments{
\item{x}{A matrix; passed to \code{as.matrix}.}
}
\description{
A step of the shdsmw_native implementation. Called by \code{.shdsmw_wls}, \code{shrinkage_msm}. See the file header for the source the module follows.
}
\value{
A vector, from \code{as.numeric}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.vec(V)
}
