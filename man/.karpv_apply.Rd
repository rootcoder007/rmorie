% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.karpv_apply}
\alias{.karpv_apply}
\title{.karpv_apply}
\usage{
.karpv_apply(op, vals)
}
\arguments{
\item{op}{One of \code{"-"}, \code{"*"}, \code{"\\\\\\\%"}, \code{"+"}.}
\item{vals}{A vector; indexed elementwise.}
}
\description{
A step of the karpV_native implementation. Called by \code{morie_karpV_evaluate}. See the file header for the source the module follows.
}
\value{
Nothing; this branch always raises.
}
\keyword{internal}
