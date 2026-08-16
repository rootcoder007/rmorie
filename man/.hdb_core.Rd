% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.hdb_core}
\alias{.hdb_core}
\title{.hdb_core}
\usage{
.hdb_core(D, n, mp)
}
\arguments{
\item{D}{A matrix; indexed by row and column.}
\item{n}{A count; the body uses it as \code{seq_len(...)}.}
\item{mp}{Numeric; passed to \code{min}.}
}
\description{
A step of the hdbsc_native implementation. Called by \code{morie_hdbsc}. See the file header for the source the module follows.
}
\value{
A vector, from \code{vapply}.
}
\keyword{internal}
