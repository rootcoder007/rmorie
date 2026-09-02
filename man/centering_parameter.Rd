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
\item{mu}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{mu_affine}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{nu}{Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{3}.}
}
\description{
A step of the mehtad_native implementation. Called by \code{morie_mehtad}, \code{solve_lp}. See the file header for the source the module follows.
}
\value{
A list with \code{sigma}, \code{ratio}, \code{nu}, \code{approximation}, \code{note}.
}
\keyword{internal}
\examples{
centering_parameter(mu = 5L, mu_affine = 5L)
}
