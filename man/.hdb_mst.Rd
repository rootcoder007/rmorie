% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.hdb_mst}
\alias{.hdb_mst}
\title{.hdb_mst}
\usage{
.hdb_mst(D, core, n)
}
\arguments{
\item{D}{A matrix; indexed by row and column.}
\item{core}{A vector; indexed elementwise.}
\item{n}{A count; the body uses it as \code{seq_len(...)}.}
}
\description{
A step of the hdbsc_native implementation. Called by \code{morie_hdbsc}. See the file header for the source the module follows.
}
\value{
The value of \code{edges}, as built in the body.
}
\keyword{internal}
