% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.karpv_replace}
\alias{.karpv_replace}
\title{.karpv_replace}
\usage{
.karpv_replace(node, path, new)
}
\arguments{
\item{node}{A list; the body reads \code{$args}, \code{$op} from it.}
\item{path}{A vector; its length is taken and its elements indexed.}
\item{new}{Passed to \code{.karpv_copy}.}
}
\description{
A step of the karpV_native implementation. Called by \code{morie_karpV}. See the file header for the source the module follows.
}
\value{
The value of \code{out}, as built in the body.
}
\keyword{internal}
