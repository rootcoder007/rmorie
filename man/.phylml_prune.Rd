% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.phylml_prune}
\alias{.phylml_prune}
\title{.phylml_prune}
\usage{
.phylml_prune(node, site, pi, u, seqs)
}
\arguments{
\item{node}{A vector; its length is taken and its elements indexed.}
\item{site}{Character; passed to \code{substr}.}
\item{pi}{Passed to \code{.phylml_prune}.}
\item{u}{Passed to \code{.phylml_prune}.}
\item{seqs}{A vector; indexed elementwise.}
}
\description{
A step of the phylml_native implementation. Called by \code{site_likelihood}. See the file header for the source the module follows.
}
\value{
The value of \code{out}, as built in the body.
}
\keyword{internal}
