% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{confusion_error}
\alias{confusion_error}
\title{confusion_error}
\usage{
confusion_error(C, labels = NULL)
}
\arguments{
\item{C}{A matrix; indexed by row and column.}
\item{labels}{Optional; may be \code{NULL}. A vector; its length is taken.}
}
\description{
A step of the attrInf_native implementation. Called by \code{morie_attrInf}. See the file header for the source the module follows.
}
\value{
The value of \code{err}, as built in the body.
}
\keyword{internal}
\examples{
err <- confusion_error(rbind(c(40, 10), c(5, 45)))
err(0L, 1L)
}
