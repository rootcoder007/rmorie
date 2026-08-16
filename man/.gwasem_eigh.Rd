% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.gwasem_eigh}
\alias{.gwasem_eigh}
\title{.gwasem_eigh}
\usage{
.gwasem_eigh(M)
}
\arguments{
\item{M}{A matrix; passed to \code{as.matrix}.}
}
\description{
A step of the gwasem_native implementation. Called by \code{morie_gwasem_reml}. See the file header for the source the module follows.
}
\value{
A list with \code{values}, \code{vectors}.
}
\keyword{internal}
\examples{
.gwasem_eigh(M = 5L)
}
