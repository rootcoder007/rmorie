% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_bx_mat}
\alias{.morie_bx_mat}
\title{.morie_bx_mat}
\usage{
.morie_bx_mat(M, name = "X")
}
\arguments{
\item{M}{Optional; may be \code{NULL}. A matrix; passed to \code{nrow}.}
\item{name}{Passed to \code{stop}. Defaults to \code{"X"}.}
}
\description{
A step of the rangayyan_class2 implementation. Called by \code{.morie_bx_chsel}, \code{BmiDec}, \code{BPursuit} and 18 others in the module. See the file header for the source the module follows.
}
\value{
The value of \code{M}, as built in the body.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.morie_bx_mat(V)
}
