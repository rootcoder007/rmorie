% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_khatri_rao_rows}
\alias{morie_khatri_rao_rows}
\title{morie_khatri_rao_rows}
\usage{
morie_khatri_rao_rows(A, B) 
}
\arguments{
\item{A}{A matrix; indexed by row and column.}
\item{B}{A matrix; indexed by row and column.}
}
\description{
A step of the gp_mvsml_ch9_14 implementation. Called by \code{Apxkern}. See the file header for the source the module follows.
}
\value{
The value of \code{out}, as built in the body.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
morie_khatri_rao_rows(V, V)
}
