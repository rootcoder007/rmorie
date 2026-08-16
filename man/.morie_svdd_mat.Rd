% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_svdd_mat}
\alias{.morie_svdd_mat}
\title{.morie_svdd_mat}
\usage{
.morie_svdd_mat(X, name)
}
\arguments{
\item{X}{A matrix; passed to \code{nrow}.}
\item{name}{See Usage.}
}
\description{
A step of the svdd_native implementation. Called by \code{morie_svdd}. See the file header for the source the module follows.
}
\value{
The value of \code{X}, as built in the body.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.morie_svdd_mat(V, V)
}
