% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{density_is_view_independent}
\alias{density_is_view_independent}
\title{density_is_view_independent}
\usage{
density_is_view_independent(model, point, directions, tol = 1e-09)
}
\arguments{
\item{model}{See Usage.}
\item{point}{Passed to \code{.nrfrad_vec}.}
\item{directions}{See Usage.}
\item{tol}{Defaults to \code{1e-09}.}
}
\description{
A step of the nrfrad_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{sigmas}, \code{max_deviation}, \code{view_independent}, \code{note}.
}
\keyword{internal}
