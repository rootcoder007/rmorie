% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.dmlqs_concat_descriptors}
\alias{.dmlqs_concat_descriptors}
\title{.dmlqs_concat_descriptors}
\usage{
.dmlqs_concat_descriptors(learned, descriptors)
}
\arguments{
\item{learned}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{descriptors}{Coerced to numeric by the body, with \code{as.numeric}.}
}
\description{
A step of the dmlqs_native implementation. Called by \code{morie_dmlqs}. See the file header for the source the module follows.
}
\value{
A list with \code{estimate}, \code{representation}, \code{learned_dim}, \code{descriptor_dim}, \code{method}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.dmlqs_concat_descriptors(V, V)
}
