% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.chronos_vec}
\alias{.chronos_vec}
\title{.chronos_vec}
\usage{
.chronos_vec(x)
}
\arguments{
\item{x}{Coerced to numeric by the body, with \code{as.numeric}.}
}
\description{
A step of the chronos_native implementation. Called by \code{forecast_summary}, \code{mean_scale}, \code{quantile_bins} and 1 others in the module. See the file header for the source the module follows.
}
\value{
A vector, from \code{as.numeric}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.chronos_vec(V)
}
