% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_wsm_trapz}
\alias{.morie_wsm_trapz}
\title{.morie_wsm_trapz}
\usage{
.morie_wsm_trapz(y, x)
}
\arguments{
\item{y}{A vector; its length is taken and its elements indexed.}
\item{x}{See Usage.}
}
\description{
A step of the wasserman_native implementation. Called by \code{morie_wasserman_entropy}, \code{morie_wasserman_kullback_leibler}, \code{morie_wasserman_posterior} and 1 others in the module. See the file header for the source the module follows.
}
\value{
A numeric value.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.morie_wsm_trapz(V, V)
}
