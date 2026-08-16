% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.dw_walk}
\alias{.dw_walk}
\title{One uniform random walk: every neighbour has probability 1/deg}
\usage{
.dw_walk(nb, start, length_, e)
}
\arguments{
\item{nb}{A vector; indexed elementwise.}
\item{start}{See Usage.}
\item{length_}{A count; the body uses it as \code{integer(...)}.}
\item{e}{Passed to \code{.ghc_unif}.}
}
\description{
A step of the deepw implementation. Called by \code{Deepw}. See the file header for the source the module follows.
}
\value{
The value of \code{[}.
}
\keyword{internal}
