% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.karpv_fnode}
\alias{.karpv_fnode}
\title{.karpv_fnode}
\usage{
.karpv_fnode(op, args)
}
\arguments{
\item{op}{Carried through into a list the body builds.}
\item{args}{Carried through into a list the body builds.}
}
\description{
A step of the karpV_native implementation. Called by \code{.karpv_copy}, \code{.karpv_grow}, \code{.karpv_replace}. See the file header for the source the module follows.
}
\value{
A list with \code{op}, \code{args}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.karpv_fnode(V, V)
}
