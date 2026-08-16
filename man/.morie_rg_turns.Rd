% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_rg_turns}
\alias{.morie_rg_turns}
\title{.morie_rg_turns}
\usage{
.morie_rg_turns(seg, threshold)
}
\arguments{
\item{seg}{A vector; its length is taken and its elements indexed.}
\item{threshold}{See Usage.}
}
\description{
A step of the rangayyan_stat implementation. Called by \code{TurnsCount}. See the file header for the source the module follows.
}
\value{
A list with \code{turns}, \code{positions}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.morie_rg_turns(V, V)
}
