% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.s03design}
\alias{.s03design}
\title{.s03design}
\usage{
.s03design(X, n)
}
\arguments{
\item{X}{Optional; may be \code{NULL}. Passed to \code{.s03mat}.}
\item{n}{A count; the body uses it as \code{matrix(...)}.}
}
\description{
A step of the helpers_s03 implementation. Called by \code{.s03drdid}, \code{.s03tmle}, \code{.tmlcic_hier_cluster_arm} and 16 others in the module. See the file header for the source the module follows.
}
\value{
The value of \code{cbind}.
}
\keyword{internal}
