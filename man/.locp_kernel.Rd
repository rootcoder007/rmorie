% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.locp_kernel}
\alias{.locp_kernel}
\title{.locp_kernel}
\usage{
.locp_kernel(name, t)
}
\arguments{
\item{name}{One of \code{"epanechnikov"}, \code{"gaussian"}, \code{"tricube"}.}
\item{t}{A vector; its length is taken and its elements indexed.}
}
\description{
A step of the locp_native implementation. Called by \code{morie_locp}. See the file header for the source the module follows.
}
\value{
The value of \code{out}, as built in the body.
}
\keyword{internal}
