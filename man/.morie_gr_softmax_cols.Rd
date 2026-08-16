% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_gr_softmax_cols}
\alias{.morie_gr_softmax_cols}
\title{Column-wise softmax (numpy softmax(axis = 0))}
\usage{
.morie_gr_softmax_cols(Z)
}
\arguments{
\item{Z}{A matrix; passed to \code{as.matrix}.}
}
\description{
A step of the geron_ml_native implementation. Called by \code{morie_geron_blip}, \code{morie_geron_softmax_function}. See the file header for the source the module follows.
}
\value{
A matrix, from \code{t}.
}
\keyword{internal}
