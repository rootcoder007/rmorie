% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.robpca_mahalanobis}
\alias{.robpca_mahalanobis}
\title{.robpca_mahalanobis}
\usage{
.robpca_mahalanobis(X, mu, C)
}
\arguments{
\item{X}{See Usage.}
\item{mu}{See Usage.}
\item{C}{A matrix; passed to \code{solve}.}
}
\description{
A step of the robpca_native implementation. Called by \code{.robpca_c_steps}, \code{.robpca_fast_mcd}, \code{morie_robpca}. See the file header for the source the module follows.
}
\value{
A numeric value.
}
\keyword{internal}
