% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.scumap_clip}
\alias{.scumap_clip}
\title{The reference implementation clips gradients to +/- 4}
\usage{
.scumap_clip(v, lim = 4)
}
\arguments{
\item{v}{See Usage.}
\item{lim}{Numeric; combined arithmetically in the body. Defaults to \code{4}.}
}
\description{
A step of the scumap_native implementation. Called by \code{morie_scumap_umap_singlecell}. See the file header for the source the module follows.
}
\value{
The value of \code{v}, as built in the body.
}
\keyword{internal}
