% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.bd_blocked}
\alias{.bd_blocked}
\title{.bd_blocked}
\usage{
.bd_blocked(p, d, Z, ch)
}
\arguments{
\item{p}{A vector; its length is taken and its elements indexed.}
\item{d}{A vector; indexed elementwise.}
\item{Z}{See Usage.}
\item{ch}{Passed to \code{.bd_desc}.}
}
\description{
A step of the causal_native implementation. Called by \code{morie_backdoor_criterion}. See the file header for the source the module follows.
}
\value{
A logical value.
}
\keyword{internal}
