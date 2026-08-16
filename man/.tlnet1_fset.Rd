% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.tlnet1_fset}
\alias{.tlnet1_fset}
\title{Private helper: friend set for unit i (1-based, excluding self)}
\usage{
.tlnet1_fset(friends, i)
}
\arguments{
\item{friends}{A vector; indexed elementwise.}
\item{i}{See Usage.}
}
\description{
A step of the tlnet1_native implementation. Called by \code{.tlnet1_count_edges}, \code{check_network_assumption}, \code{friend_summary} and 1 others in the module. See the file header for the source the module follows.
}
\value{
The value of \code{setdiff}.
}
\keyword{internal}
\examples{
.tlnet1_fset(friends = c(1, 2, 3, 4, 5, 6, 7, 8), i = 5L)
}
