% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{centering_parameter}
\alias{centering_parameter}
\title{centering_parameter}
\usage{
centering_parameter(mu, mu_affine, nu = 3)
}
\arguments{
\item{mu}{See Usage.}
\item{mu_affine}{See Usage.}
\item{nu}{Defaults to \code{3}.}
}
\description{
Part of the mehtad_native implementation; see the file header for the source it follows.
}
\value{
A list with \code{sigma}, \code{ratio}, \code{nu}, \code{approximation}, \code{note}.
}
\keyword{internal}
\examples{
centering_parameter(mu = 5L, mu_affine = 5L)
}
