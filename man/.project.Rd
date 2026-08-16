% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.project}
\alias{.project}
\title{.project}
\usage{
.project(values, basis, n, ridge)
}
\arguments{
\item{values}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{basis}{Optional; may be \code{NULL}. A matrix; passed to \code{as.matrix}.}
\item{n}{See Usage.}
\item{ridge}{Numeric; combined arithmetically in the body.}
}
\description{
A step of the tmldyn_native implementation. Called by \code{sequential_blips}. See the file header for the source the module follows.
}
\value{
A vector, from \code{as.numeric}.
}
\keyword{internal}
