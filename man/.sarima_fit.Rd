% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.sarima_fit}
\alias{.sarima_fit}
\title{.sarima_fit}
\usage{
.sarima_fit(y, order = c(0, 1, 1), seasonal_order = c(0, 1, 1), s = 12, method = "ml", start = NULL)
}
\arguments{
\item{y}{Passed to \code{.sarima_package}.}
\item{order}{A vector; indexed elementwise. Defaults to \code{c(0, 1, 1)}.}
\item{seasonal_order}{A vector; indexed elementwise. Defaults to \code{c(0, 1, 1)}.}
\item{s}{Passed to \code{.sarima_package}. Defaults to \code{12}.}
\item{method}{One of \code{"css"}, \code{"moment"}, \code{"uls"}. Defaults to \code{"ml"}.}
\item{start}{Defaults to \code{NULL}.}
}
\description{
A step of the sarima_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
The value of \code{.sarima_package}.
}
\keyword{internal}
