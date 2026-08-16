% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{volume_render}
\alias{volume_render}
\title{volume_render}
\usage{
volume_render(sigma, colour, t)
}
\arguments{
\item{sigma}{Passed to \code{.nrfrad_vec}.}
\item{colour}{Passed to \code{.nrfrad_mat}.}
\item{t}{Passed to \code{.nrfrad_vec}.}
}
\description{
A step of the nrfrad_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{colour}, \code{weights}, \code{accumulated_alpha}, \code{transmittance_final}, \code{note}.
}
\keyword{internal}
\examples{
volume_render(sigma = c(1, 2, 3, 4, 5, 6, 7, 8), colour = c(1, 2, 3, 4, 5, 6, 7, 8), t = c(1, 2, 3, 4, 5, 6, 7, 8))
}
