% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.remlfn_groups}
\alias{.remlfn_groups}
\title{Group y values by group label, preserving first-appearance order}
\usage{
.remlfn_groups(y, group)
}
\arguments{
\item{y}{A vector; its length is taken and its elements indexed.}
\item{group}{A vector; its length is taken and its elements indexed.}
}
\description{
A step of the remlfn_native implementation. Called by \code{.remlfn_ranova}, \code{morie_remlfn}. See the file header for the source the module follows.
}
\value{
A list with \code{keys}, \code{gs}.
}
\keyword{internal}
