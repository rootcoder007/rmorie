% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.try_fit}
\alias{.try_fit}
\title{.try_fit}
\usage{
.try_fit(y, X, order, seasonal_order, s, constant, method)
}
\arguments{
\item{y}{Passed to \code{.sarimax_fit}.}
\item{X}{Optional; may be \code{NULL}. Passed to \code{.sarimax_fit}.}
\item{order}{A vector; indexed elementwise.}
\item{seasonal_order}{A vector; indexed elementwise.}
\item{s}{Passed to \code{.sarimax_fit}.}
\item{constant}{A flag; the body branches on it.}
\item{method}{Passed to \code{.sarimax_fit}.}
}
\description{
A step of the sarimax_native implementation. Called by \code{auto_order}. See the file header for the source the module follows.
}
\value{
The value of \code{tryCatch}.
}
\keyword{internal}
