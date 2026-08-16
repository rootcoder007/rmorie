% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.gwasem_solve}
\alias{.gwasem_solve}
\title{.gwasem_solve}
\usage{
.gwasem_solve(A, b)
}
\arguments{
\item{A}{A matrix; passed to \code{solve}.}
\item{b}{A matrix; passed to \code{solve}.}
}
\description{
A step of the gwasem_native implementation. Called by \code{.gwasem_loglik}, \code{.gwasem_reml_delta}, \code{morie_gwasem} and 1 others in the module. See the file header for the source the module follows.
}
\value{
A vector, from \code{as.numeric}.
}
\keyword{internal}
