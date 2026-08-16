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
\item{y}{See Usage.}
\item{order}{Defaults to \code{c(0, 1, 1)}.}
\item{seasonal_order}{Defaults to \code{c(0, 1, 1)}.}
\item{s}{Defaults to \code{12}.}
\item{method}{Defaults to \code{"ml"}.}
\item{start}{Defaults to \code{NULL}.}
}
\description{
Part of the sarima_native implementation; see the file header for the source it follows.
}
\value{
The value of \code{.sarima_package}.
}
\keyword{internal}
