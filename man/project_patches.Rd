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
\item{patch_features}{Iterated over elementwise, with \code{lapply}.}
\item{W}{A vector; its length is taken and its elements indexed.}
\item{b}{Optional; may be \code{NULL}. Coerced to numeric by the body, with \code{as.numeric}.}
}
\description{
A step of the llavx_native implementation. Called by \code{morie_llavx}. See the file header for the source the module follows.
}
\value{
The value of \code{lapply}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
project_patches(V, V)
}
