% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.tf_smooth2d}
\alias{.tf_smooth2d}
\title{.tf_smooth2d}
\usage{
.tf_smooth2d(tfd, tlen, flen)
}
\arguments{
\item{tfd}{A matrix; indexed by row and column.}
\item{tlen}{Passed to \code{gauss}.}
\item{flen}{Passed to \code{gauss}.}
}
\description{
A step of the rangayyan_tf implementation. Called by \code{Gtfd}. See the file header for the source the module follows.
}
\value{
The value of \code{out}, as built in the body.
}
\keyword{internal}
