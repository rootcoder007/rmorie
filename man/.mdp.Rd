% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.mdp}
\alias{.mdp}
\title{.mdp}
\usage{
.mdp(T, gamma)
}
\arguments{
\item{T}{A vector; its length is taken and its elements indexed.}
\item{gamma}{The body requires: birl: gamma must be in [0, 1).}
}
\description{
A step of the birl_native implementation. Called by \code{birl}, \code{policy_iteration}, \code{policy_values} and 2 others in the module. See the file header for the source the module follows.
}
\value{
A list with \code{nS}, \code{nA}.
}
\keyword{internal}
