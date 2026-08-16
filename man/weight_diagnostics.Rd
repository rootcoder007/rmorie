% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{weight_diagnostics}
\alias{weight_diagnostics}
\title{weight_diagnostics}
\usage{
weight_diagnostics(weights)
}
\arguments{
\item{weights}{See Usage.}
}
\description{
A step of the bhltmsm_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{mean}, \code{max}, \code{min}, \code{effective_n}, \code{n}, \code{efficiency}, \code{mean_near_one}, \code{note}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
weight_diagnostics(V)
}
