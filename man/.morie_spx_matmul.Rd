% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_spx_matmul}
\alias{.morie_spx_matmul}
\title{.morie_spx_matmul}
\usage{
.morie_spx_matmul(A, B)
}
\arguments{
\item{A}{A matrix; indexed by row and column.}
\item{B}{A matrix; indexed by row and column.}
}
\description{
A step of the sp_fill implementation. Called by \code{.morie_spx_lstsq}, \code{.morie_spx_sarneg2}, \code{MantelZ} and 2 others in the module. See the file header for the source the module follows.
}
\value{
The value of \code{out}, as built in the body.
}
\keyword{internal}
