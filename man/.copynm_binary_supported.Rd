% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.copynm_binary_supported}
\alias{.copynm_binary_supported}
\title{Edge-effect undo: is `cut` a viable BINARY change-point for v?}
\usage{
.copynm_binary_supported(v, cut, alpha, perms, e)
}
\arguments{
\item{v}{A vector; its length is taken and its elements indexed.}
\item{cut}{A count; the body uses it as \code{seq_len(...)}.}
\item{alpha}{Numeric; combined arithmetically in the body.}
\item{perms}{Numeric; combined arithmetically in the body.}
\item{e}{Passed to \code{.copynm_shuffle}.}
}
\description{
A step of the copynm_native implementation. Called by \code{copynm}. See the file header for the source the module follows.
}
\value{
A logical value.
}
\keyword{internal}
