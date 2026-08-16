% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.rdkfp_subgraphs}
\alias{.rdkfp_subgraphs}
\title{.rdkfp_subgraphs}
\usage{
.rdkfp_subgraphs(B, minpath, maxpath, branched)
}
\arguments{
\item{B}{A list; the body reads \code{$a}, \code{$i}, \code{$j} from it.}
\item{minpath}{Passed to \code{>=}.}
\item{maxpath}{Passed to \code{<=}.}
\item{branched}{A flag; the body branches on it.}
}
\description{
A step of the rdkfp implementation. Called by \code{Rdkfp}. See the file header for the source the module follows.
}
\value{
The value of \code{out}, as built in the body.
}
\keyword{internal}
