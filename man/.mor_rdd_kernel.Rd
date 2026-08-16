% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.mor_rdd_kernel}
\alias{.mor_rdd_kernel}
\title{.mor_rdd_kernel}
\usage{
.mor_rdd_kernel(name, u)
}
\arguments{
\item{name}{One of \code{"triangular"}, \code{"uniform"}.}
\item{u}{Numeric; passed to \code{abs}.}
}
\description{
A step of the causrdd_native implementation. Called by \code{morie_causrdd}. See the file header for the source the module follows.
}
\value{
Nothing; this branch always raises.
}
\keyword{internal}
