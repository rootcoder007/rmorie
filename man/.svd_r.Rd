% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.svd_r}
\alias{.svd_r}
\title{Returns U, s, V (V transposed, like np.linalg.svd with full matrices)}
\usage{
.svd_r(a)
}
\arguments{
\item{a}{A matrix; passed to \code{as.matrix}.}
}
\description{
A step of the andmnh_native implementation. Called by \code{.singular_value_adjust}. See the file header for the source the module follows.
}
\value{
A list with \code{u}, \code{s}, \code{v}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.svd_r(V)
}
