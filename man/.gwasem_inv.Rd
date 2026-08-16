% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.gwasem_inv}
\alias{.gwasem_inv}
\title{.gwasem_inv}
\usage{
.gwasem_inv(A)
}
\arguments{
\item{A}{A matrix; passed to \code{solve}.}
}
\description{
A step of the gwasem_native implementation. Called by \code{morie_gwasem}. See the file header for the source the module follows.
}
\value{
A matrix, from \code{solve}.
}
\keyword{internal}
