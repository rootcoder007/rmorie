% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.robpca_matrix}
\alias{.robpca_matrix}
\title{.robpca_matrix}
\usage{
.robpca_matrix(X, name = "X")
}
\arguments{
\item{X}{Optional; may be \code{NULL}. A matrix; passed to \code{dim}.}
\item{name}{Passed to \code{sprintf}. Defaults to \code{"X"}.}
}
\description{
A step of the robpca_native implementation. Called by \code{morie_robpca}, \code{outlyingness}. See the file header for the source the module follows.
}
\value{
The value of \code{X}, as built in the body.
}
\keyword{internal}
\examples{
M <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 2)
.robpca_matrix(M)
}
