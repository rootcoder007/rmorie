% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{covariate_kink_test}
\alias{covariate_kink_test}
\title{covariate_kink_test}
\usage{
covariate_kink_test(V, Z, kink, bandwidth, order = 2L, kernel = "triangular")
}
\arguments{
\item{V}{See Usage.}
\item{Z}{See Usage.}
\item{kink}{See Usage.}
\item{bandwidth}{See Usage.}
\item{order}{Defaults to \code{2L}.}
\item{kernel}{Defaults to \code{"triangular"}.}
}
\description{
Part of the bnskt2_native implementation; see the file header for the source it follows.
}
\value{
A list with \code{slope_change}, \code{slope_right}, \code{slope_left}, \code{n_right}, \code{n_left}, \code{interpretation}.
}
\keyword{internal}
