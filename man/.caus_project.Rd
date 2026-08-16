% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.caus_project}
\alias{.caus_project}
\title{P_Z M by least squares rather than a formed inverse: Z\\'Z is}
\usage{
.caus_project(Z, M)
}
\arguments{
\item{Z}{A matrix; passed to \code{\%*\%}.}
\item{M}{A matrix; passed to \code{as.matrix}.}
}
\description{
routinely near-singular when instruments are correlated, and inverting it explicitly turns a warning into a wrong answer.
}
\value{
The value of \code{\%*\%}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.caus_project(V, V)
}
