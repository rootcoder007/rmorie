% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_rg_scatter}
\alias{.morie_rg_scatter}
\title{Sum of (x - mu)(x - mu)^T: the SCATTER, not divided by n}
\usage{
.morie_rg_scatter(X, mu)
}
\arguments{
\item{X}{A matrix; passed to \code{as.matrix}.}
\item{mu}{See Usage.}
}
\description{
A step of the rangayyan_class implementation. Called by \code{FishLda}, \code{SepIndex}. See the file header for the source the module follows.
}
\value{
The value of \code{\%*\%}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.morie_rg_scatter(V, V)
}
