% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.phylby_replace_branch}
\alias{.phylby_replace_branch}
\title{.phylby_replace_branch}
\usage{
.phylby_replace_branch(node, path, value)
}
\arguments{
\item{node}{See Usage.}
\item{path}{A vector; its length is taken and its elements indexed.}
\item{value}{Passed to \code{.phylby_replace_branch}.}
}
\description{
A step of the phylby_native implementation. Called by \code{.phylby_step}. See the file header for the source the module follows.
}
\value{
The value of \code{parts}, as built in the body.
}
\keyword{internal}
\examples{
.phylby_replace_branch(node = list(a = 1, b = 2), path = 5L, value = c(1, 2, 3, 4, 5, 6, 7, 8))
}
