% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.hibrid_chol}
\alias{.hibrid_chol}
\title{.hibrid_chol}
\usage{
.hibrid_chol(A)
}
\arguments{
\item{A}{A matrix; indexed by row and column.}
}
\description{
A step of the hibrid_native implementation. Called by \code{.hibrid_reml_at}. See the file header for the source the module follows.
}
\value{
The value of \code{L}, as built in the body.
}
\keyword{internal}
