% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{neighbours}
\alias{neighbours}
\title{neighbours}
\usage{
neighbours(order, seasonal_order, constant, s)
}
\arguments{
\item{order}{A vector; indexed elementwise.}
\item{seasonal_order}{A vector; indexed elementwise.}
\item{constant}{A flag; the body branches on it.}
\item{s}{Coerced to integer by the body, with \code{as.integer}.}
}
\description{
A step of the sarimax_native implementation. Called by \code{auto_order}. See the file header for the source the module follows.
}
\value{
The value of \code{out}, as built in the body.
}
\keyword{internal}
