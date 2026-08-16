% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_w4c_pca_svd}
\alias{.morie_w4c_pca_svd}
\title{PCA by SVD of the centred (optionally scaled) matrix}
\usage{
.morie_w4c_pca_svd(X, k, center = TRUE, scale = FALSE)
}
\arguments{
\item{X}{See Usage.}
\item{k}{See Usage.}
\item{center}{Defaults to \code{TRUE}.}
\item{scale}{Defaults to \code{FALSE}.}
}
\description{
Part of the geron_w4c_native implementation; see the file header for the source it follows.
}
\value{
A list with \code{components}, \code{scores}, \code{explained_variance}, \code{explained_variance_ratio}, \code{singular_values}, \code{Xc}.
}
\keyword{internal}
