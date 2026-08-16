% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.robpca_det_from_chol}
\alias{.robpca_det_from_chol}
\title{.robpca_det_from_chol}
\usage{
.robpca_det_from_chol(C)
}
\arguments{
\item{C}{A matrix; passed to \code{chol}.}
}
\description{
A step of the robpca_native implementation. Called by \code{.robpca_c_steps}, \code{.robpca_fast_mcd}. See the file header for the source the module follows.
}
\value{
The value of \code{prod}.
}
\keyword{internal}
