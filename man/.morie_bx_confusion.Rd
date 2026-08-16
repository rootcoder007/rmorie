% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_bx_confusion}
\alias{.morie_bx_confusion}
\title{.morie_bx_confusion}
\usage{
.morie_bx_confusion(true, pred)
}
\arguments{
\item{true}{See Usage.}
\item{pred}{See Usage.}
}
\description{
A step of the rangayyan_class2 implementation. Called by \code{CadPipe}, \code{PvcBayes}. See the file header for the source the module follows.
}
\value{
A list with \code{tp}, \code{tn}, \code{fp}, \code{fn}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.morie_bx_confusion(V, V)
}
