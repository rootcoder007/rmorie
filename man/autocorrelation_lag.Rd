% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{autocorrelation_lag}
\alias{autocorrelation_lag}
\title{autocorrelation_lag}
\usage{
autocorrelation_lag(y, threshold = NULL)
}
\arguments{
\item{y}{A vector; its length is taken and its elements indexed.}
\item{threshold}{Optional; may be \code{NULL}. Passed to \code{is.null}.}
}
\description{
A step of the lyapun_native implementation. Called by \code{divergence_curve}, \code{morie_lyapun}. See the file header for the source the module follows.
}
\value{
A numeric value.
}
\keyword{internal}
