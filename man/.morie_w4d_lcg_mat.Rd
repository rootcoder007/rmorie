% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_w4d_lcg_mat}
\alias{.morie_w4d_lcg_mat}
\title{.morie_w4d_lcg_mat}
\usage{
.morie_w4d_lcg_mat(nr, nc, seed, scale = 0.1)
}
\arguments{
\item{nr}{A count; the body uses it as \code{matrix(...)}.}
\item{nc}{A count; the body uses it as \code{matrix(...)}.}
\item{seed}{See Usage.}
\item{scale}{Numeric; combined arithmetically in the body. Defaults to \code{0.1}.}
}
\description{
A step of the geron_w4d_native implementation. Called by \code{.morie_w4d_sae_train_layer}. See the file header for the source the module follows.
}
\value{
A matrix, from \code{matrix}.
}
\keyword{internal}
