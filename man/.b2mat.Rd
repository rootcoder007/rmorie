% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.b2mat}
\alias{.b2mat}
\title{.b2mat}
\usage{
.b2mat(a)
}
\arguments{
\item{a}{A matrix; the body checks with \code{is.matrix}.}
}
\description{
A step of the b2ot implementation. Called by \code{.b2sinkhorn}, \code{Bottomup}, \code{Gppost} and 13 others in the module. See the file header for the source the module follows.
}
\value{
The value of \code{m}, as built in the body.
}
\keyword{internal}
