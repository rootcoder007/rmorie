% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.mor_rdk_subgraphs}
\alias{.mor_rdk_subgraphs}
\title{.mor_rdk_subgraphs}
\usage{
.mor_rdk_subgraphs(a, bd, minpath, maxpath, branched)
}
\arguments{
\item{a}{A count; the body uses it as \code{seq_len(...)}.}
\item{bd}{A list; the body reads \code{$i}, \code{$j} from it.}
\item{minpath}{See Usage.}
\item{maxpath}{A count; the body uses it as \code{seq_len(...)}.}
\item{branched}{A flag; the body branches on it.}
}
\description{
A step of the rdkfp_native implementation. Called by \code{morie_rdkfp}. See the file header for the source the module follows.
}
\value{
The value of \code{out}, as built in the body.
}
\keyword{internal}
