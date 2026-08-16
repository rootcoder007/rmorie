% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.karpv_grow}
\alias{.karpv_grow}
\title{.karpv_grow}
\usage{
.karpv_grow(e, functions, terminals, erc, d, full)
}
\arguments{
\item{e}{Passed to \code{.karpv_random_terminal}.}
\item{functions}{A vector; its length is taken and its elements indexed.}
\item{terminals}{A vector; its length is taken.}
\item{erc}{Optional; may be \code{NULL}. Passed to \code{.karpv_random_terminal}.}
\item{d}{Numeric; combined arithmetically in the body.}
\item{full}{A flag; the body branches on it.}
}
\description{
A step of the karpV_native implementation. Called by \code{morie_karpV}, \code{morie_karpV_ramped}. See the file header for the source the module follows.
}
\value{
The value of \code{.karpv_fnode}.
}
\keyword{internal}
