% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{rkd_estimate}
\alias{rkd_estimate}
\title{rkd_estimate}
\usage{
rkd_estimate(V, Y, kink, bandwidth, order = 2L, kernel = "triangular", policy_slope_change = NULL, B = NULL, fuzzy = FALSE)
}
\arguments{
\item{V}{See Usage.}
\item{Y}{See Usage.}
\item{kink}{Passed to \code{.side_fit}.}
\item{bandwidth}{Passed to \code{.side_fit}.}
\item{order}{Defaults to \code{2L}.}
\item{kernel}{Passed to \code{.side_fit}. Defaults to \code{"triangular"}.}
\item{policy_slope_change}{Defaults to \code{NULL}.}
\item{B}{Defaults to \code{NULL}.}
\item{fuzzy}{A flag; the body branches on it. Defaults to \code{FALSE}.}
}
\description{
A step of the bnskt2_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{estimate}, \code{tau}, \code{outcome_kink}, \code{policy_kink}, \code{slope_right}, \code{slope_left}, \code{n_right}, \code{n_left}, \code{bandwidth}, \code{order}, \code{kernel}, \code{fuzzy}, \code{denominator_source}, \code{method}, \code{requires}.
}
\keyword{internal}
