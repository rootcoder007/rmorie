% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.sctraj_interp}
\alias{.sctraj_interp}
\title{.sctraj_interp}
\usage{
.sctraj_interp(s, curve, u)
}
\arguments{
\item{s}{A vector; its length is taken and its elements indexed.}
\item{curve}{A matrix; indexed by row and column.}
\item{u}{Numeric; combined arithmetically in the body.}
}
\description{
A step of the sctraj_native implementation. Called by \code{morie_sctraj}, \code{morie_sctraj_average_curve}. See the file header for the source the module follows.
}
\value{
The value of \code{[}.
}
\keyword{internal}
\examples{
.sctraj_interp(s = 5L, curve = matrix(c(1, 2, 3, 4, 5, 6), nrow = 2), u = 5L)
}
