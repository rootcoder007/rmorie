% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{time_weights}
\alias{time_weights}
\title{time_weights}
\usage{
time_weights(Y, treated, t_post) 
}
\arguments{
\item{Y}{Passed to \code{.causscd_grid}.}
\item{treated}{Passed to \code{.causscd_grid}.}
\item{t_post}{A count; the body uses it as \code{seq_len(...)}.}
}
\description{
A step of the causscd_native implementation. Called by \code{sdid}. See the file header for the source the module follows.
}
\value{
A list with \code{weights}, \code{intercept}.
}
\keyword{internal}
\examples{
set.seed(1)
Y <- lapply(1:8, function(i) rnorm(10, 10))
treated <- c(rep(0L, 6), 1L, 1L)
str(time_weights(Y, treated, t_post = 6), max.level = 1)
}
