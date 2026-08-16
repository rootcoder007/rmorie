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
\item{V}{Passed to \code{.side_fit}.}
\item{Z}{Passed to \code{.side_fit}.}
\item{kink}{Passed to \code{.side_fit}.}
\item{bandwidth}{Passed to \code{.side_fit}.}
\item{order}{Defaults to \code{2L}.}
\item{kernel}{Passed to \code{.side_fit}. Defaults to \code{"triangular"}.}
}
\description{
A step of the bnskt2_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{slope_change}, \code{slope_right}, \code{slope_left}, \code{n_right}, \code{n_left}, \code{interpretation}.
}
\keyword{internal}
