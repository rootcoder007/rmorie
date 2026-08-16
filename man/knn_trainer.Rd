% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{knn_trainer}
\alias{knn_trainer}
\title{knn_trainer}
\usage{
knn_trainer(k = 1L, smoothing = 0.001)
}
\arguments{
\item{k}{A count; the body uses it as \code{seq_len(...)}. Defaults to \code{1L}.}
\item{smoothing}{A count; the body uses it as \code{rep(...)}. Defaults to \code{0.001}.}
}
\description{
A step of the memb_native implementation. Called by \code{morie_memb}. See the file header for the source the module follows.
}
\value{
The value of \code{train}, as built in the body.
}
\keyword{internal}
\examples{
knn_trainer()
}
