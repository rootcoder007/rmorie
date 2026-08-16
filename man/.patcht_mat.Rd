% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.patcht_mat}
\alias{.patcht_mat}
\title{.patcht_mat}
\usage{
.patcht_mat(M)
}
\arguments{
\item{M}{A matrix; the body checks with \code{is.matrix}.}
}
\description{
A step of the patchT_native implementation. Called by \code{channel_independent_tokens}, \code{channel_mixed_tokens}, \code{patchtst_encode}. See the file header for the source the module follows.
}
\value{
One of two values, depending on the branch taken.
}
\keyword{internal}
