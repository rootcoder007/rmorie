% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.jntlo_loss}
\alias{.jntlo_loss}
\title{.jntlo_loss}
\usage{
.jntlo_loss(kind, y, yh)
}
\arguments{
\item{kind}{One of \code{"binary"}, \code{"cont"}, \code{"count"}, \code{"ordinal"}.}
\item{y}{A vector; its length is taken and its elements indexed.}
\item{yh}{A vector; indexed elementwise.}
}
\description{
A step of the jntlO implementation. Called by \code{Jntlo}. See the file header for the source the module follows.
}
\value{
Nothing; this branch always raises.
}
\keyword{internal}
