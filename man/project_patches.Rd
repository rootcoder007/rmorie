% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{project_patches}
\alias{project_patches}
\title{project_patches}
\usage{
project_patches(patch_features, W, b = NULL)
}
\arguments{
\item{patch_features}{See Usage.}
\item{W}{See Usage.}
\item{b}{Defaults to \code{NULL}.}
}
\description{
Part of the llavx_native implementation; see the file header for the source it follows.
}
\value{
The value of \code{lapply}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
project_patches(V, V)
}
