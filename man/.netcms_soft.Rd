% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.netcms_soft}
\alias{.netcms_soft}
\title{.netcms_soft}
\usage{
.netcms_soft(x, t)
}
\arguments{
\item{x}{Numeric; passed to \code{abs}.}
\item{t}{Numeric; combined arithmetically in the body.}
}
\description{
A step of the netcms_native implementation. Called by \code{.netcms_lasso}. See the file header for the source the module follows.
}
\value{
A numeric value.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.netcms_soft(V, V)
}
