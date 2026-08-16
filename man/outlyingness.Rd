% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{outlyingness}
\alias{outlyingness}
\title{outlyingness}
\usage{
outlyingness(X, h = NULL, n_dirs = 250L, seed = 17L)
}
\arguments{
\item{X}{Passed to \code{.robpca_matrix}.}
\item{h}{Optional; may be \code{NULL}. Passed to \code{is.null}.}
\item{n_dirs}{Passed to \code{.robpca_directions}. Defaults to \code{250L}.}
\item{seed}{Passed to \code{.robpca_directions}. Defaults to \code{17L}.}
}
\description{
A step of the robpca_native implementation. Called by \code{morie_robpca}. See the file header for the source the module follows.
}
\value{
A list with \code{outl}, \code{exact_fit_direction}.
}
\keyword{internal}
\examples{
M <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 2)
outlyingness(M)
}
