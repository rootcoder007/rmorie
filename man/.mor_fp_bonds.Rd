% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.mor_fp_bonds}
\alias{.mor_fp_bonds}
\title{Bond list: 0-based (i, j, order) triples in the Python enumeration}
\usage{
.mor_fp_bonds(adjacency)
}
\arguments{
\item{adjacency}{A matrix; passed to \code{as.matrix}.}
}
\description{
order (i ascending, then j > i ascending)
}
\value{
A list with \code{a}, \code{i}, \code{j}, \code{o}.
}
\keyword{internal}
