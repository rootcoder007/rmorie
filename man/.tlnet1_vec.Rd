% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.tlnet1_vec}
\alias{.tlnet1_vec}
\title{Private helper: coerce to a flat numeric vector (equivalent of k.vec)}
\usage{
.tlnet1_vec(values)
}
\arguments{
\item{values}{A matrix; the body checks with \code{is.matrix}.}
}
\description{
A step of the tlnet1_native implementation. Called by \code{friend_summary}, \code{network_influence_variance}. See the file header for the source the module follows.
}
\value{
One of two values, depending on the branch taken.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.tlnet1_vec(V)
}
