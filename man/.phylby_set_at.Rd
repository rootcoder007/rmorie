% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.phylby_set_at}
\alias{.phylby_set_at}
\title{.phylby_set_at}
\usage{
.phylby_set_at(node, path, value)
}
\arguments{
\item{node}{See Usage.}
\item{path}{A vector; its length is taken and its elements indexed.}
\item{value}{Passed to \code{.phylby_set_at}.}
}
\description{
A step of the phylby_native implementation. Called by \code{.phylby_nni_neighbours}. See the file header for the source the module follows.
}
\value{
The value of \code{parts}, as built in the body.
}
\keyword{internal}
\examples{
.phylby_set_at(node = list(a = 1, b = 2), path = 5L, value = c(1, 2, 3, 4, 5, 6, 7, 8))
}
