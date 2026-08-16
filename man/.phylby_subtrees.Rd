% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.phylby_subtrees}
\alias{.phylby_subtrees}
\title{.phylby_subtrees}
\usage{
.phylby_subtrees(node, path = integer(0))
}
\arguments{
\item{node}{A vector; its length is taken and its elements indexed.}
\item{path}{Passed to \code{c}. Defaults to \code{integer(0)}.}
}
\description{
A step of the phylby_native implementation. Called by \code{.phylby_nni_neighbours}. See the file header for the source the module follows.
}
\value{
The value of \code{out}, as built in the body.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.phylby_subtrees(V)
}
