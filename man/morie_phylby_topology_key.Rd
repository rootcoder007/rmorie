% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_phylby_topology_key}
\alias{morie_phylby_topology_key}
\title{morie_phylby_topology_key}
\usage{
morie_phylby_topology_key(tree)
}
\arguments{
\item{tree}{Passed to \code{.phylby_topology_key}.}
}
\description{
A step of the phylby_native implementation. Called by \code{morie_phylby}. See the file header for the source the module follows.
}
\value{
The value of \code{.phylby_topology_key}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
morie_phylby_topology_key(V)
}
