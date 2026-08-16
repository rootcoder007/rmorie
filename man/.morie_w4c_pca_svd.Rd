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
\item{X}{A matrix; passed to \code{nrow}.}
\item{k}{A count; the body uses it as \code{seq_len(...)}.}
\item{center}{A flag; the body branches on it. Defaults to \code{TRUE}.}
\item{scale}{A flag; the body branches on it. Defaults to \code{FALSE}.}
}
\description{
A step of the geron_w4c_native implementation. Called by \code{morie_geron_principal_components}. See the file header for the source the module follows.
}
\value{
A list with \code{components}, \code{scores}, \code{explained_variance}, \code{explained_variance_ratio}, \code{singular_values}, \code{Xc}.
}
\keyword{internal}
