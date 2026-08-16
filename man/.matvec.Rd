% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.matvec}
\alias{.matvec}
\title{.matvec}
\usage{
.matvec(W, x)
}
\arguments{
\item{W}{A matrix; passed to \code{\%*\%}.}
\item{x}{Coerced to vector by the body, with \code{as.vector}.}
}
\description{
A step of the explor_native implementation. Called by \code{explor}. See the file header for the source the module follows.
}
\value{
A vector, from \code{as.numeric}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.matvec(V, V)
}
