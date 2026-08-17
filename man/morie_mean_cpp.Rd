% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_mean_cpp}
\alias{morie_mean_cpp}
\title{morie_mean_cpp}
\usage{
morie_mean_cpp(x)
}
\arguments{
\item{x}{Passed to \code{.Call}.}
}
\description{
A step of the RcppExports implementation. Called by \code{morie_mean}. See the file header for the source the module follows.
}
\value{
The value of \code{.Call}.
}
\keyword{internal}
\examples{
set.seed(1)
r <- morie_mean_cpp(x = rnorm(10)); TRUE
}
