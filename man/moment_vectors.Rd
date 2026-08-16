% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{moment_vectors}
\alias{moment_vectors}
\title{moment_vectors}
\usage{
moment_vectors(e, X)
}
\arguments{
\item{e}{A vector; its length is taken.}
\item{X}{A matrix; passed to \code{nrow}.}
}
\description{
A step of the andmnh_native implementation. Called by \code{andrews_monahan_hac}. See the file header for the source the module follows.
}
\value{
The value of \code{V}, as built in the body.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
moment_vectors(V, V)
}
