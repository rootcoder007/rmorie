% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.karpv_collect}
\alias{.karpv_collect}
\title{.karpv_collect}
\usage{
.karpv_collect(node, path, out)
}
\arguments{
\item{node}{A list; the body reads \code{$args} from it.}
\item{path}{See Usage.}
\item{out}{A vector; its length is taken and its elements indexed.}
}
\description{
A step of the karpV_native implementation. Called by \code{.karpv_pick_point}. See the file header for the source the module follows.
}
\value{
The value of \code{out}, as built in the body.
}
\keyword{internal}
