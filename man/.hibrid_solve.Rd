% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.hibrid_solve}
\alias{.hibrid_solve}
\title{.hibrid_solve}
\usage{
.hibrid_solve(L, b)
}
\arguments{
\item{L}{A matrix; indexed by row and column.}
\item{b}{A vector; indexed elementwise.}
}
\description{
A step of the hibrid_native implementation. Called by \code{.hibrid_reml_at}, \code{morie_hibrid_hibrid_prediction}. See the file header for the source the module follows.
}
\value{
The value of \code{x}, as built in the body.
}
\keyword{internal}
