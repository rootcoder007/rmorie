% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.t2_agg}
\alias{.t2_agg}
\title{.t2_agg}
\usage{
.t2_agg(name, vals)
}
\arguments{
\item{name}{One of \code{"max"}, \code{"mean"}, \code{"min"}, \code{"std"}.}
\item{vals}{A vector; its length is taken.}
}
\description{
A step of the tail2_t01 implementation. Called by \code{PnaAgg}. See the file header for the source the module follows.
}
\value{
Nothing; this branch always raises.
}
\keyword{internal}
