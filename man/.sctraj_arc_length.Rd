% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.sctraj_arc_length}
\alias{.sctraj_arc_length}
\title{.sctraj_arc_length}
\usage{
.sctraj_arc_length(curve)
}
\arguments{
\item{curve}{A matrix; indexed by row and column.}
}
\description{
A step of the sctraj_native implementation. Called by \code{morie_sctraj}, \code{morie_sctraj_principal_curve}. See the file header for the source the module follows.
}
\value{
The value of \code{s}, as built in the body.
}
\keyword{internal}
\examples{
M <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 2)
.sctraj_arc_length(M)
}
