% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.limmav_lowess}
\alias{.limmav_lowess}
\title{.limmav_lowess}
\usage{
.limmav_lowess(x, y, span = 0.5, iterations = 3)
}
\arguments{
\item{x}{A vector; its length is taken and its elements indexed.}
\item{y}{A vector; its length is taken and its elements indexed.}
\item{span}{Numeric; combined arithmetically in the body. Defaults to \code{0.5}.}
\item{iterations}{Coerced to integer by the body, with \code{as.integer}. Defaults to \code{3}.}
}
\description{
A step of the limmav_native implementation. Called by \code{.limmav_voom_weights}. See the file header for the source the module follows.
}
\value{
The value of \code{out}, as built in the body.
}
\keyword{internal}
