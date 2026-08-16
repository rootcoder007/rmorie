% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.gpztploglik}
\alias{.gpztploglik}
\title{.gpztploglik}
\usage{
.gpztploglik(y_positive, mu)
}
\arguments{
\item{y_positive}{Passed to \code{.gpflat}.}
\item{mu}{Numeric; passed to \code{log}.}
}
\description{
A step of the helpers_gp_core implementation. Called by \code{.gpzapbestsplit}, \code{Msm325}. See the file header for the source the module follows.
}
\value{
A numeric value.
}
\keyword{internal}
