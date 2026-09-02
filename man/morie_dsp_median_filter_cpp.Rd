% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_dsp_median_filter_cpp}
\alias{morie_dsp_median_filter_cpp}
\title{morie_dsp_median_filter_cpp}
\usage{
morie_dsp_median_filter_cpp(x, kernel_size) 
}
\arguments{
\item{x}{Passed to \code{.Call}.}
\item{kernel_size}{Passed to \code{.Call}.}
}
\description{
A step of the RcppExports implementation. Called by \code{morie_dsp_median_filter}. See the file header for the source the module follows.
}
\value{
The value of \code{.Call}.
}
\keyword{internal}
\examples{
set.seed(1)
r <- morie_dsp_median_filter_cpp(x = rnorm(10), kernel_size = 8L); TRUE
}
