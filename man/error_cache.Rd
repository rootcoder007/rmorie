% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{error_cache}
\alias{error_cache}
\title{error_cache}
\usage{
error_cache(alpha, y, K, b)
}
\arguments{
\item{alpha}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{y}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{K}{A matrix; indexed by row and column.}
\item{b}{Coerced to numeric by the body, with \code{as.numeric}.}
}
\description{
A step of the smoopt_native implementation. Called by \code{smo_platt}. See the file header for the source the module follows.
}
\value{
The value of \code{out}, as built in the body.
}
\keyword{internal}
