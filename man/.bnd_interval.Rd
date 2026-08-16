% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.bnd_interval}
\alias{.bnd_interval}
\title{.bnd_interval}
\usage{
.bnd_interval(moments, name)
}
\arguments{
\item{moments}{A matrix; passed to \code{as.matrix}.}
\item{name}{See Usage.}
}
\description{
A step of the helpers_bnd implementation. Called by \code{Bndinf}, \code{Bnsiii}. See the file header for the source the module follows.
}
\value{
A list with \code{yl}, \code{yu}.
}
\keyword{internal}
