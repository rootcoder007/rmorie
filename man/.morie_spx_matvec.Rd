% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_spx_matvec}
\alias{.morie_spx_matvec}
\title{.morie_spx_matvec}
\usage{
.morie_spx_matvec(A, b)
}
\arguments{
\item{A}{A matrix; indexed by row and column.}
\item{b}{Numeric; combined arithmetically in the body.}
}
\description{
A step of the sp_fill implementation. Called by \code{.morie_spx_lstsq}, \code{.morie_spx_sarneg2}, \code{.morie_spx_topeigs} and 4 others in the module. See the file header for the source the module follows.
}
\value{
A vector, from \code{vapply}.
}
\keyword{internal}
