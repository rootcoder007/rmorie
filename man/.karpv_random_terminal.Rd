% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.karpv_random_terminal}
\alias{.karpv_random_terminal}
\title{.karpv_random_terminal}
\usage{
.karpv_random_terminal(e, terminals, erc)
}
\arguments{
\item{e}{Passed to \code{.karpv_below}.}
\item{terminals}{A vector; its length is taken and its elements indexed.}
\item{erc}{Optional; may be \code{NULL}. A vector; indexed elementwise.}
}
\description{
A step of the karpV_native implementation. Called by \code{.karpv_grow}. See the file header for the source the module follows.
}
\value{
The value of \code{.karpv_tnode}.
}
\keyword{internal}
