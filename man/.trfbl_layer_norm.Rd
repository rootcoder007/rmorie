% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.trfbl_layer_norm}
\alias{.trfbl_layer_norm}
\title{Internal helper: Trfbl Layer Norm}
\usage{
.trfbl_layer_norm(x, eps = 1e-05)
}
\description{
Internal helper: Trfbl Layer Norm
}
\value{
The value of \code{sweep}.
}
\keyword{internal}
\examples{
M <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 2)
.trfbl_layer_norm(M)
}
