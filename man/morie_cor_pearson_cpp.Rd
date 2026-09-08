% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_cor_pearson_cpp}
\alias{morie_cor_pearson_cpp}
\title{morie_cor_pearson_cpp}
\usage{
morie_cor_pearson_cpp(x, y) 
}
\arguments{
\item{x}{Passed to \code{.Call}.}
\item{y}{Passed to \code{.Call}.}
}
\description{
A step of the RcppExports implementation. Called by \code{morie_cor_pearson}. See the file header for the source the module follows.
}
\value{
The value of \code{.Call}.
}
\keyword{internal}
\examples{
set.seed(1)
r <- morie_cor_pearson_cpp(x = rnorm(10), y = rnorm(10)); TRUE
}
