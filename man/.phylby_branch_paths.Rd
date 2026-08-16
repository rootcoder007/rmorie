% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.phylby_branch_paths}
\alias{.phylby_branch_paths}
\title{.phylby_branch_paths}
\usage{
.phylby_branch_paths(node, path = integer(0))
}
\arguments{
\item{node}{A vector; its length is taken and its elements indexed.}
\item{path}{Defaults to \code{integer(0)}.}
}
\description{
A step of the phylby_native implementation. Called by \code{.phylby_log_posterior}, \code{.phylby_step}. See the file header for the source the module follows.
}
\value{
The value of \code{out}, as built in the body.
}
\keyword{internal}
