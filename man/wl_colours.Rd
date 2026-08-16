% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{wl_colours}
\alias{wl_colours}
\title{wl_colours}
\usage{
wl_colours(adj, n, rounds = 2, initial = NULL)
}
\arguments{
\item{adj}{See Usage.}
\item{n}{See Usage.}
\item{rounds}{Defaults to \code{2}.}
\item{initial}{Defaults to \code{NULL}.}
}
\description{
Part of the sortP_native implementation; see the file header for the source it follows.
}
\value{
The value of \code{c}, as built in the body.
}
\keyword{internal}
\examples{
wl_colours(adj = data.frame(x = c(1, 2, 3, 4), y = c(2, 4, 5, 9)), n = 5L)
}
